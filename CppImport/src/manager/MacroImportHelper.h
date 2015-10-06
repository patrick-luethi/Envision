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
		void setProject(OOModel::Project* project);
		void setSourceManager(const clang::SourceManager* sourceManager);
		void setPreprocessor(const clang::Preprocessor* preprocessor);

		void macroGeneration();

		void finalize();

		void removeIncompleteExpansions();

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

		bool getUnexpandedNameWithQualifiers(clang::SourceLocation loc, QString* result);
		bool nameSeparator(QString candidate);

		void correctFormalArgType(clang::NamedDecl* namedDecl, OOModel::FormalArgument* arg);
		void correctCastType(clang::Expr* expr, OOModel::CastExpression* cast);
		void correctFormalResultType(clang::FunctionDecl* method, OOModel::FormalResult* current);
		void correctMethodCall(clang::Expr* expr, OOModel::MethodCallExpression* methodCall);
		void correctReferenceExpression(clang::SourceLocation loc, OOModel::ReferenceExpression* reference);
		void correctExplicitTemplateInst(clang::ClassTemplateSpecializationDecl* specializationDecl,
													OOModel::ReferenceExpression* reference);
		void correctIntegerLiteral(clang::IntegerLiteral* intLit, OOModel::IntegerLiteral* original);
		void correctStringLiteral(clang::StringLiteral* strLit, OOModel::StringLiteral* original);
		void correctNamedDecl(clang::Decl* decl, Model::Node* node);

		QVector<MacroArgumentLocation> getArgumentHistory(clang::SourceRange range);
		QVector<MacroArgumentLocation> getArgumentHistory(Model::Node* node);
		void getAllArguments(Model::Node* node, QVector<MacroArgumentInfo>* result, NodeMapping* mapping);

		QHash<Model::Node*, QSet<MacroExpansion*>> expansionCache_;

		OOModel::Project* root_{};

		ClangHelper::Token getUnexpToken(clang::SourceLocation start);
		QString hashDefinition(const clang::MacroDirective* md);

		void applyLexicalTransformations(Model::Node* node, NodeMapping* mapping);

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

		QHash<Model::Node*, QString> lexicalTransform_;

		bool isExpansionception(clang::SourceLocation loc);
		QString getUnexpandedSpelling(clang::SourceRange range);

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
