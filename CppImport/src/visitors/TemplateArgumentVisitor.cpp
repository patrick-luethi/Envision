/***********************************************************************************************************************
 **
 ** Copyright (c) 2011, 2014 ETH Zurich
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
#include "TemplateArgumentVisitor.h"

namespace CppImport {

TemplateArgumentVisitor::TemplateArgumentVisitor
(ExpressionVisitor* vis, CppImportUtilities* util, CppImportLogger* log)
: exprVisitor_{vis}, utils_{util}, log_{log}
{}

OOModel::FormalTypeArgument* TemplateArgumentVisitor::translateTemplateArg(clang::Decl* d)
{
	// remove reference lastTranslatedArg
	lastTranslatedTypeArg_ = nullptr;
	TraverseDecl(d);
	// TODO: exprVisitor_->baseVisitor_->trMngr_->mapping_[d] = lastTranslatedTypeArg_;
	Q_ASSERT(lastTranslatedTypeArg_);
	return lastTranslatedTypeArg_;
}

bool TemplateArgumentVisitor::VisitDecl(clang::Decl* decl)
{
	if (decl)
	{
		log_->writeError(className_, decl, CppImportLogger::Reason::OTHER,
							  "Can not handle this decl with this visitor");
		lastTranslatedTypeArg_ = new OOModel::FormalTypeArgument("#ERROR");
		return true;
	}
	return RecursiveASTVisitor<TemplateArgumentVisitor>::VisitDecl(decl);
}

bool TemplateArgumentVisitor::TraverseTemplateTypeParmDecl(clang::TemplateTypeParmDecl* templateParm)
{
	lastTranslatedTypeArg_ = new OOModel::FormalTypeArgument(QString::fromStdString(templateParm->getNameAsString()));
	if (templateParm->hasDefaultArgument())
		lastTranslatedTypeArg_->setSubTypeOfExpression(utils_->translateQualifiedType(templateParm->getDefaultArgument(),
																						 templateParm->getLocStart()));
	return true;
}

bool TemplateArgumentVisitor::TraverseNonTypeTemplateParmDecl(clang::NonTypeTemplateParmDecl* nonTypeTemplateParm)
{
	lastTranslatedTypeArg_ = new OOModel::FormalTypeArgument
(QString::fromStdString(nonTypeTemplateParm->getNameAsString()));
	lastTranslatedTypeArg_->setSubTypeOfExpression(utils_->translateQualifiedType(nonTypeTemplateParm->getType(),
																					 nonTypeTemplateParm->getLocStart()));
	if (nonTypeTemplateParm->hasDefaultArgument())
		lastTranslatedTypeArg_->setSuperTypeOfExpression(exprVisitor_->translateExpression
																		 (nonTypeTemplateParm->getDefaultArgument()));
	return true;
}

}
