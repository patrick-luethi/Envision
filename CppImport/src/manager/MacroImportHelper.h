/***********************************************************************************************************************
 **
 ** Copyright (c) 2011, 2015 ETH Zurich
 ** All rights reserved.
 **
 ** Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 ** following conditions are met:
 **
 **    * Redistributions of source code must retain the above copyright notice, this list of conditions and the
 **      following disclaimer.
 **    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the
 **      following disclaimer in the documentation and/or other materials provided with the distribution.
 **    * Neither the name of the ETH Zurich nor the names of its contributors may be used to endorse or promote products
 **      derived from this software without specific prior written permission.
 **
 **
 ** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 ** INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 ** DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 ** SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 ** SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 ** WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 ** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 **
 **********************************************************************************************************************/

#pragma once

#include "cppimport_api.h"

#include "ClangHelper.h"
#include "AstMapping.h"
#include "MacroExpansion.h"
#include "NodeMapping.h"
#include "MacroArgumentLocation.h"
#include "MacroArgumentInfo.h"

#include "OOModel/src/allOOModelNodes.h"
#include "clang/Lex/MacroArgs.h"

namespace CppImport {

class CPPIMPORT_API MacroImportHelper
{
	public:
		MacroImportHelper(OOModel::Project* project) : root_(project), lexicalHelper_(this) {}

		void setSourceManager(const clang::SourceManager* sourceManager);
		void setPreprocessor(const clang::Preprocessor* preprocessor);

		void macroGeneration();

		void finalize();

		QVector<MacroExpansion*> expansions_;

		QHash<const clang::MacroDirective*, QString> definitions_;

		void addMacroDefinition(QString name, const clang::MacroDirective* md);
		void addMacroExpansion(clang::SourceRange sr, const clang::MacroDirective* md, const clang::MacroArgs* args);

		void mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode);
		void mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode);

		ClangHelper* clang();
		AstMapping* astMapping();

		QString getDefinitionName(const clang::MacroDirective* md);

		QVector<MacroExpansion*> getTopLevelExpansions();

		MacroExpansion* getExpansion(clang::SourceLocation loc);
		MacroExpansion* getExpansion(OOModel::MetaCallExpression* metaCall);
		MacroExpansion* getImmediateExpansion(clang::SourceLocation loc);
		QSet<MacroExpansion*> getExpansion(Model::Node* node);

		QVector<Model::Node*> getTopLevelNodes(MacroExpansion* expansion);

		QString hashExpansion(MacroExpansion* expansion);
		bool shouldCreateMetaCall(MacroExpansion* expansion);

		QVector<Model::Node*> getNodes(MacroExpansion* expansion, NodeMapping* mapping);

		OOModel::Declaration* getActualContext(MacroExpansion* expansion);

		QVector<MacroArgumentLocation> getArgumentHistory(clang::SourceRange range);
		QVector<MacroArgumentLocation> getArgumentHistory(Model::Node* node);
		void getAllArguments(Model::Node* node, QVector<MacroArgumentInfo>* result, NodeMapping* mapping);

		QHash<Model::Node*, QSet<MacroExpansion*>> expansionCache_;

		OOModel::Project* root_;

		QString hashDefinition(const clang::MacroDirective* md);


		class LexicalHelper
		{
			public:
				LexicalHelper(MacroImportHelper* mih) : mih_(mih) {}

				bool getUnexpandedNameWithQualifiers(clang::SourceLocation loc, QString* result)
				{
					auto token = getUnexpToken(loc);

					*result = token.value();
					if (result->startsWith("#")) return true; // TODO: wth?

					QRegularExpression regularExpression("^(\\w|##)*$");
					auto match = regularExpression.match(token.value());
					if (!match.hasMatch())
					{
						*result = "";
						return false;
					}

					while (true)
					{
						auto sepa = token.next();
						if (!nameSeparator(sepa.value()))
							break;

						auto identifier = sepa.next();
						if (identifier.value() == ",")
							break;

						*result += sepa.value() + identifier.value();
						token = identifier.next();
					}

					return true;
				}

				bool nameSeparator(QString candidate)
				{
					return candidate == "::" || candidate == "." || candidate == "<" || candidate == ">" || candidate == "->";
				}

				bool isExpansionception(clang::SourceLocation loc)
				{
					if (loc.isMacroID())
						if (auto immediateExpansion = mih_->getImmediateExpansion(loc))
							return mih_->clang()->sourceManager()->getImmediateExpansionRange(loc).first !=
									immediateExpansion->range.getBegin();

					return false;
				}

				ClangHelper::Token getUnexpToken(clang::SourceLocation start)
				{
					auto loc = isExpansionception(start) ?
								mih_->clang()->sourceManager()->getImmediateExpansionRange(start).first : start;

					return ClangHelper::Token(mih_->clang(), mih_->clang()->sourceManager()->getSpellingLoc(loc));
				}

				QString getUnexpandedSpelling(clang::SourceRange range)
				{
					clang::SourceLocation start, end;

					if (isExpansionception(range.getBegin()))
						start = mih_->clang()->sourceManager()->getImmediateExpansionRange(range.getBegin()).first;
					else
						start = range.getBegin();

					if (isExpansionception(range.getEnd()))
						end = mih_->clang()->sourceManager()->getImmediateExpansionRange(range.getEnd()).second;
					else
						end = range.getEnd();

					return mih_->clang()->getSpelling(start, end);
				}

				void correctFormalArgType(clang::NamedDecl* namedDecl, OOModel::FormalArgument* original)
				{
					if (!mih_->clang()->isMacroRange(namedDecl->getSourceRange())) return;

					auto token = getUnexpToken(namedDecl->getSourceRange().getBegin());
					auto typeTokens = token.type();

					auto identifier = token.toString(typeTokens);
					auto nextToken = typeTokens.last()->next();

					while (nextToken.isWhitespace() || nextToken.isEmpty()) nextToken = nextToken.next();

					if (nextToken.value() == "*") // TODO: this won't work with multipointers
					{
						if (auto ptrExpr = DCast<OOModel::PointerTypeExpression>(original->typeExpression()))
						if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
							lexicalTransform_.insert(refExpr, identifier);
					}
					else if (nextToken.value() == "&")
					{
						if (auto ptrExpr = DCast<OOModel::ReferenceTypeExpression>(original->typeExpression()))
						if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
							lexicalTransform_.insert(refExpr, identifier);
					}
					else
						lexicalTransform_.insert(original->typeExpression(), identifier);
				}

				void correctFormalResultType(clang::FunctionDecl* method,
																				OOModel::FormalResult* original)
				{
					if (!mih_->clang()->isMacroRange(method->getReturnTypeSourceRange())) return;

					auto token = getUnexpToken(method->getReturnTypeSourceRange().getBegin());
					auto typeTokens = token.type();

					auto identifier = token.toString(typeTokens);
					auto nextToken = typeTokens.last()->next();

					while (nextToken.isWhitespace() || nextToken.isEmpty()) nextToken = nextToken.next();

					if (nextToken.value() == "*")
					{
						if (auto ptrExpr = DCast<OOModel::PointerTypeExpression>(original->typeExpression()))
						if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
							lexicalTransform_.insert(refExpr, identifier);
					}
					else if (nextToken.value() == "&")
					{
						if (auto ptrExpr = DCast<OOModel::ReferenceTypeExpression>(original->typeExpression()))
						if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
							lexicalTransform_.insert(refExpr, identifier);
					}
					else
						lexicalTransform_.insert(original->typeExpression(), identifier);
				}

				void correctMethodCall(clang::Expr* expr, OOModel::MethodCallExpression* methodCall)
				{
					if (!expr->getExprLoc().isMacroID()) return;

					auto ref = DCast<OOModel::ReferenceExpression>(methodCall->callee());
					if (!ref) return;

					auto token = getUnexpToken(expr->getSourceRange().getBegin());

					/*
					 * these conditions intend to skip all implict type initializations.
					 * the locations don't map to the actual call in those situations because they don't exist in the source (implicit)
					 * we therefore have to skip them.
					 */
					if (!getUnexpandedSpelling(expr->getSourceRange()).contains("(") && methodCall->arguments()->size() == 0) return;
					if (token.value() == "{") return;

					/*
					 * an exception to the above are implicit QString initializations.
					 * since those might contain vital information we have to handle those.
					 */
					if (ref->name() == "QString" && methodCall->arguments()->size() == 1)
					{
						if (auto sLit = DCast<OOModel::StringLiteral>(methodCall->arguments()->first()))
						{
							lexicalTransform_.insert(sLit, getUnexpandedSpelling(expr->getSourceRange()));
							return;
						}
					}

					QStack<OOModel::ReferenceExpression*> refs;
					while (true)
					{
						for (auto i = ref->typeArguments()->size() - 1; i >= 0; i--)
							if (auto r = DCast<OOModel::ReferenceExpression>(ref->typeArguments()->at(i)))
								refs.push(r);
							else
								return;

						refs.push(ref);

						if (ref->prefix())
						{
							ref = DCast<OOModel::ReferenceExpression>(ref->prefix());
							if (!ref) return;
						}
						else
							break;
					}

					auto identTokens = token.identifier();

					if (!identTokens.empty())
					{
						token = token.identifier().last()->next();
						lexicalTransform_.insert(refs.pop(), token.toString(identTokens));
					}
					else
					{
						// TODO: maybe examine this deeper (only intended to happen for global scope)
						refs.pop();
						token = token.next();
					}

					while (!refs.empty())
					{
						while (nameSeparator(token.value()))
							token = token.next();

						identTokens = token.identifier();

						if (!identTokens.empty())
						{
							lexicalTransform_.insert(refs.pop(), token.toString(identTokens));
							token = identTokens.last()->next();
						}
						else
						{
							// TODO: why?
							refs.pop();
							token = token.next();
						}
					}
				}

				void correctReferenceExpression(clang::SourceLocation loc, OOModel::ReferenceExpression* original)
				{
					if (loc.isMacroID())
						if (isExpansionception(loc))
						{
							auto token = getUnexpToken(loc);
							lexicalTransform_.insert(original, token.toString(token.identifier()));
						}
				}

				void correctExplicitTemplateInst(clang::ClassTemplateSpecializationDecl* specializationDecl,
																					 OOModel::ReferenceExpression* original)
				{
					if (!mih_->clang()->isMacroRange(specializationDecl->getSourceRange()))	return;

					auto spelling = mih_->clang()->getSpelling(specializationDecl->getLocation(),
																		  specializationDecl->getSourceRange().getEnd());

					// TODO: improve parsing to handle more complicated cases.
					QRegularExpression regularExpression("^(\\w+)<(\\w+::\\w+)>$");

					auto match = regularExpression.match(spelling);
					if (match.hasMatch())
					{
						lexicalTransform_.insert(original, match.captured(1));

						Q_ASSERT(original->typeArguments()->size() == 1);

						auto typeArg = DCast<OOModel::ReferenceExpression>(original->typeArguments()->first());
						Q_ASSERT(typeArg);
						Q_ASSERT(!typeArg->prefix());
						qDebug() << match.captured(2);
						lexicalTransform_.insert(typeArg, match.captured(2));
					}
					else
					{
						qDebug() << "could not correct explicit template instantiation: " << spelling;
					}
				}

				void correctStringLiteral(clang::StringLiteral* strLit, OOModel::StringLiteral* original)
				{
					if (!strLit->getLocStart().isMacroID()) return;

					auto rawValue = getUnexpandedSpelling(strLit->getSourceRange());

					lexicalTransform_.insert(original, rawValue);
				}

				void correctIntegerLiteral(clang::IntegerLiteral* intLit, OOModel::IntegerLiteral* original)
				{
					if (!intLit->getLocation().isMacroID()) return;

					auto token = getUnexpToken(intLit->getLocStart());

					lexicalTransform_.insert(original, token.value());
				}

				void correctCastType(clang::Expr* expr, OOModel::CastExpression* original)
				{
					if (!mih_->clang()->isMacroRange(expr->getSourceRange())) return;

					auto spelling = mih_->clang()->getSpelling(expr->getSourceRange());

					QRegularExpression regularExpression("static_cast<\\s*((\\w|\\*|##)+)\\s*>");
					auto match = regularExpression.match(spelling);

					if (match.hasMatch())
					{
						auto typeExpression = match.captured(1);

						if (typeExpression.endsWith("*"))
						{
							if (auto ptrExpr = DCast<OOModel::PointerTypeExpression>(original->castType()))
							if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
								lexicalTransform_.insert(refExpr, typeExpression);
						}
						else
							lexicalTransform_.insert(original->castType(), typeExpression);
					}
				}

				void correctNamedDecl(clang::Decl* decl, Model::Node* node)
				{
					if (!mih_->clang()->isMacroRange(decl->getSourceRange())) return;

					if (auto ooDecl = DCast<OOModel::Declaration>(node))
					{
						if (auto namedDecl = clang::dyn_cast<clang::NamedDecl>(decl))
						{
							auto token = getUnexpToken(namedDecl->getLocation());

							lexicalTransform_.insert(ooDecl, token.toString(token.qualifiedIdentifier()));
						}
						else
							Q_ASSERT(false && "not implemented");
					}
					else
						Q_ASSERT(false && "not implemented");
				}

				void applyLexicalTransformations(Model::Node* node, NodeMapping* mapping)
				{
					if (lexicalTransform_.contains(mapping->original(node)))
					{
						auto transformed = lexicalTransform_.value(mapping->original(node));

						if (auto ref = DCast<OOModel::ReferenceExpression>(node))
							ref->setName(transformed);
						else if (auto decl = DCast<OOModel::Declaration>(node))
							decl->setName(transformed);
						else if (auto strLit = DCast<OOModel::StringLiteral>(node))
							strLit->setValue(transformed);
						else
							qDebug() << "Unhandled transformed node type" << node->typeName();
					}

					for (auto child : node->children())
						applyLexicalTransformations(child, mapping);
				}
			private:
				MacroImportHelper* mih_;
				QHash<Model::Node*, QString> lexicalTransform_;
		} lexicalHelper_;

	private:
		class StaticStuff
		{
			public:
				static void orderNodes(QVector<Model::Node*>& input)
				{
					qSort(input.begin(), input.end(),
							[](Model::Node* e1, Model::Node* e2)
							{
								if (auto commonAncestor = e1->lowestCommonAncestor(e2))
									if (auto list = DCast<Model::List>(commonAncestor))
									{
										int index1 = -1;
										for (auto c : list->children())
											if (c == e1 || c->isAncestorOf(e1))
											{
												index1 = list->indexOf(c);
												break;
											}

										int index2 = -1;
										for (auto c : list->children())
											if (c == e2 || c->isAncestorOf(e2))
											{
												index2 = list->indexOf(c);
												break;
											}

										return index1 < index2;
									}

								return true;
							});
				}

				static bool validContext(Model::Node* node)
				{
					if (DCast<OOModel::Project>(node))
						return true;
					else if (DCast<OOModel::Module>(node))
						return true;
					else if (DCast<OOModel::Class>(node))
						return true;
					else if (DCast<OOModel::Method>(node))
						return true;
					else
						return false;
				}

				static OOModel::Declaration* getActualContext(Model::Node* node)
				{
					auto current = node->parent();

					while (current)
					{
						if (auto result = DCast<OOModel::Project>(current))
							return result;
						else if (auto result = DCast<OOModel::Module>(current))
							return result;
						else if (auto result = DCast<OOModel::Class>(current))
							return result;
						else if (auto result = DCast<OOModel::Method>(current))
							return result;
						else
							current = current->parent();
					}

					Q_ASSERT(false);
				}

				static OOModel::Declaration* createContext(OOModel::Declaration* actualContext)
				{
					if (DCast<OOModel::Project>(actualContext))
						return new OOModel::Project("Context");
					else if (DCast<OOModel::Module>(actualContext))
						return new OOModel::Module("Context");
					else if (DCast<OOModel::Class>(actualContext))
						return new OOModel::Class("Context");
					else if (DCast<OOModel::Method>(actualContext))
						return new OOModel::Method("Context");

					Q_ASSERT(false);
				}

				static Model::Node* cloneWithMapping(Model::Node* node, NodeMapping* mapping)
				{
					auto clone = node->clone();

					QList<Model::Node*> info;
					buildMappingInfo(node, &info);
					useMappingInfo(clone, &info, mapping);

					return clone;
				}

			private:
				static void buildMappingInfo(Model::Node* node, QList<Model::Node*>* info)
				{
					info->push_back(node);

					for (auto child : node->children())
						buildMappingInfo(child, info);
				}

				static void useMappingInfo(Model::Node* node,
																	  QList<Model::Node*>* info,
																	  NodeMapping* mapping)
				{
					mapping->add(info->front(), node);
					info->pop_front();

					for (auto child : node->children())
						useMappingInfo(child, info, mapping);
				}
		};

		MacroExpansion* currentXMacroParent {};
		ClangHelper clang_;
		AstMapping astMapping_;

		QHash<QString, OOModel::MetaCallExpression*> metaCallDuplicationPrevention_;

		struct {
				QSet<Model::Node*> nodes;
				QHash<Model::Node*, MacroExpansion*> metaCalls;
		} finalizationInfo;

		QHash<QString, OOModel::MetaDefinition*> metaDefinitions_;

		void clear();

		void handleMacroExpansion(QVector<Model::Node*> nodes, MacroExpansion* expansion, NodeMapping* mapping,
										  QVector<MacroArgumentInfo>& arguments, QHash<MacroExpansion*, Model::Node*>* splices);

		void createMetaDef(QVector<Model::Node*> nodes, MacroExpansion* expansion, NodeMapping* mapping,
								 QVector<MacroArgumentInfo>& arguments, QHash<MacroExpansion*, Model::Node*>* splices);

		void removeNode(Model::Node* node);
		void getChildrenNotBelongingToExpansion(Model::Node* node, MacroExpansion* expansion,
															 NodeMapping* mapping, QVector<Model::Node*>* result,
															 QHash<MacroExpansion*, Model::Node*>* splices);

		void addNodeToMetaDef(Model::Node* cloned, OOModel::MetaDefinition* metaDef);
		void insertArgumentSplices(NodeMapping* mapping, NodeMapping* childMapping, QVector<MacroArgumentInfo>& arguments);
		bool removeUnownedNodes(Model::Node* cloned, MacroExpansion* expansion,
										NodeMapping* mapping, QHash<MacroExpansion*, Model::Node*>* splices);
		void addChildMetaCalls(OOModel::MetaDefinition* metaDef, MacroExpansion* expansion,
									  NodeMapping* childMapping, QHash<MacroExpansion*, Model::Node*>* splices);
		OOModel::MetaCallExpression* containsMetaCall(Model::Node* node);
		MacroExpansion*getMatchingXMacroExpansion(Model::Node* node);
		OOModel::MetaDefinition* createXMacroMetaDef(MacroExpansion* xMacroExpansionH,
																	MacroExpansion* xMacroExpansionCpp);
		MacroExpansion* partialBeginMacroChild(MacroExpansion* expansion);

		void getChildrenBelongingToExpansion(MacroExpansion* expansion, QVector<Model::Node*>* result);
};

}
