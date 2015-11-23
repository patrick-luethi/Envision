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

#include "../../cppexport_api.h"
#include "../../exporter/ExportError.h"
#include "../Visitor.h"

namespace Export {
	class SourceDir;
	class SourceFile;
	class SourceFragment;
}

namespace CppExport {

class CPPEXPORT_API DeclarationVisitorHeader : public Visitor {
	public:
		using Visitor::Visitor;

		Export::SourceDir* visitProject(OOModel::Project* project, Export::SourceDir* parent = nullptr);
		Export::SourceDir* visitModule(OOModel::Module* module, Export::SourceDir* parent);
		Export::SourceFile* visitTopLevelClass(OOModel::Class* classs, Export::SourceDir* parent);

		Export::SourceFragment* visit(OOModel::Declaration* declaration);

		Export::SourceFragment* visit(OOModel::Class* classs);
		Export::SourceFragment* visit(OOModel::Method* method);

		Export::SourceFragment* visit(OOModel::NameImport* nameImport);
		Export::SourceFragment* visit(OOModel::VariableDeclaration* vd);

		Export::SourceFragment* visit(OOModel::ExplicitTemplateInstantiation* eti);
		Export::SourceFragment* visit(OOModel::TypeAlias* ta);

		Export::SourceFragment* printAnnotationsAndModifiers(OOModel::Declaration* declaration);
};

}
