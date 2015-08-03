/***********************************************************************************************************************
**
** Copyright (c) 2011, 2014 ETH Zurich
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
** following conditions are met:
**
**    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
**      disclaimer.
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
***********************************************************************************************************************/

#include "OOVisualizationPlugin.h"
#include "SelfTest/src/SelfTestSuite.h"

#include "MethodAddOn.h"
#include "../src/declarations/VMethod.h"

#include "OOModel/src/allOOModelNodes.h"

#include "InteractionBase/src/autocomplete/AutoComplete.h"

#include "VisualizationBase/src/VisualizationManager.h"
#include "VisualizationBase/src/Scene.h"
#include "VisualizationBase/src/views/MainView.h"
#include "VisualizationBase/src/renderer/ModelRenderer.h"
#include "VisualizationBase/src/items/VComposite.h"
#include "VisualizationBase/src/items/VText.h"
#include "VisualizationBase/src/items/VList.h"
#include "VisualizationBase/src/node_extensions/Position.h"
#include "VisualizationBase/src/ViewItemManager.h"

#include "ModelBase/src/model/TreeManager.h"
#include "ModelBase/src/nodes/UsedLibrary.h"

#include "OOModel/src/EnvisionMetaDefinition.h"

using namespace OOModel;
using namespace Visualization;
using namespace Comments;

namespace OOVisualization {

Class* addFoo(Project* parent)
{
	auto c = new Class("Foo");
	if (parent) parent->classes()->append(c);

	Method* main = new Method("bar");
	c->methods()->append(main);

	ExpressionStatement* metaCallStmt = new ExpressionStatement();
	/*MetaCallExpression* metaCall = new MetaCallExpression("ATTRIBUTE");
		 metaCall->arguments()->append(new OOModel::PrimitiveTypeExpression(PrimitiveType::PrimitiveTypes::BOOLEAN));
		 metaCall->arguments()->append(new ReferenceExpression("subDeclarations"));
		 metaCall->arguments()->append(new ReferenceExpression("setSubDeclarations"));*/

	//MetaCallExpression* metaCall = new MetaCallExpression("DECLARE_TYPE_ID");

	/*MetaCallExpression* metaCall = new MetaCallExpression("PREFIX");
	metaCall->arguments()->append(new ReferenceExpression("dontcare"));*/

	MetaCallExpression* metaCall = new MetaCallExpression("BEGIN_STANDARD_EXPRESSION_VISUALIZATION_BASE");
	metaCall->arguments()->append(new ReferenceExpression("OOVISUALIZATION_API"));
	metaCall->arguments()->append(new ReferenceExpression("VDeleteExpression"));
	metaCall->arguments()->append(new ReferenceExpression("DeleteExpression",
												new ReferenceExpression("OOVisualization")));
	metaCall->arguments()->append(new ReferenceExpression("isArray"));

	auto list = new ArrayInitializer();
	{
		auto v1 = new MetaCallExpression("PREFIX");
		v1->arguments()->append(new BooleanLiteral(true));
		list->values()->append(v1);
		auto v2 = new MetaCallExpression("OPERAND");
		v2->arguments()->append(new ReferenceExpression("expr"));
		list->values()->append(v2);
	}
	metaCall->arguments()->append(list);

	metaCallStmt->setExpression(metaCall);
	main->items()->append(metaCallStmt);

	metaCall->generate();

	return c;
}

TEST(OOVisualizationPlugin, JavaLibraryAndHelloWorldTest)
{
	auto prj = new Project("CodeGenerationTest");

	// nodeMacros.h
	//prj->subDeclarations()->append(EnvisionMetaDefinition::attribute());
	//prj->subDeclarations()->append(EnvisionMetaDefinition::private_attribute());

	// typeIdMacros.h
	//prj->subDeclarations()->append(EnvisionMetaDefinition::declare_type_id_common());
	//prj->subDeclarations()->append(EnvisionMetaDefinition::declare_type_id());

	// ItemMacros.h
	//prj->subDeclarations()->append(EnvisionMetaDefinition::item_common_custom_stylename());

	// StandardExpressionVisualizations.h
	prj->subDeclarations()->append(EnvisionMetaDefinition::begin_standard_expression_visualization_style_h());
	prj->subDeclarations()->append(EnvisionMetaDefinition::begin_standard_expression_visualization_h());
	prj->subDeclarations()->append(EnvisionMetaDefinition::begin_standard_enumeration_expression_visualization_h());
	prj->subDeclarations()->append(EnvisionMetaDefinition::begin_standard_flag_expression_visualization_h());
	prj->subDeclarations()->append(EnvisionMetaDefinition::expression_part_h());
	prj->subDeclarations()->append(EnvisionMetaDefinition::prefix_h());
	prj->subDeclarations()->append(EnvisionMetaDefinition::infix_h());
	prj->subDeclarations()->append(EnvisionMetaDefinition::infix2_h());
	prj->subDeclarations()->append(EnvisionMetaDefinition::postfix_h());
	prj->subDeclarations()->append(EnvisionMetaDefinition::operand_h());
	prj->subDeclarations()->append(EnvisionMetaDefinition::wrapped_operand_h());

	// StandardExpressionVisualizations.cpp
	prj->subDeclarations()->append(EnvisionMetaDefinition::begin_standard_expression_visualization_all_cpp());
	prj->subDeclarations()->append(EnvisionMetaDefinition::begin_standard_expression_visualization_cpp());
	prj->subDeclarations()->append(EnvisionMetaDefinition::begin_standard_enumeration_expression_visualization_cpp());
	prj->subDeclarations()->append(EnvisionMetaDefinition::begin_standard_flag_expression_visualization_cpp());
	prj->subDeclarations()->append(EnvisionMetaDefinition::operand_cpp());
	prj->subDeclarations()->append(EnvisionMetaDefinition::wrapped_operand_cpp());
	prj->subDeclarations()->append(EnvisionMetaDefinition::preinpostfix_cpp());
	prj->subDeclarations()->append(EnvisionMetaDefinition::prefix_cpp());
	prj->subDeclarations()->append(EnvisionMetaDefinition::infix_cpp());
	prj->subDeclarations()->append(EnvisionMetaDefinition::infix2_cpp());
	prj->subDeclarations()->append(EnvisionMetaDefinition::postfix_cpp());

	// StandardExpressionVisualizations merged
	prj->subDeclarations()->append(EnvisionMetaDefinition::begin_standard_expression_visualization_base());

	addFoo(prj);

	////////////////////////////////////////////////// Set Scene
	Model::Node* top_level = prj;

	auto manager = new Model::TreeManager(top_level);
	manager->setName("HelloWorld");

	auto mainScene = VisualizationManager::instance().mainScene();
	mainScene->addTopLevelNode(top_level);
	mainScene->listenToTreeManager(manager);

	// Center view
	mainScene->updateNow();
	for (auto v : mainScene->views())
		if (auto mainView = dynamic_cast<Visualization::MainView*>(v))
		{
			mainView->centerOn(mainScene->sceneRect().center());
			break;
		}

	// Watch files
	VisualizationManager::instance().mainScene()->addRefreshActionFunction(
				[top_level](Scene* scene){
		scene->viewItems()->removeAllViewItems();
		scene->setMainCursor(nullptr);
		clearAllStyleSets();
		scene->addTopLevelNode(top_level);
	});

	QStringList filesToWatch;
	std::function<void (QString)> allFiles = [&filesToWatch, &allFiles](QString dirPath)
	{
		auto dir = QDir{dirPath + '/'};
		auto entries = dir.entryInfoList(QDir::AllEntries | QDir::NoDot | QDir::NoDotDot);
		for (auto entry : entries)
		{
			if (entry.isFile()) filesToWatch.append(entry.absoluteFilePath());
			else if (entry.isDir()) allFiles(entry.absoluteFilePath());
		}
	};

	allFiles("styles");
	auto watcher = new QFileSystemWatcher(filesToWatch);

	auto onFileChange = [watcher](const QString& fileName)
	{
		watcher->addPath(fileName);

		QKeyEvent *eventPress = new QKeyEvent ( QEvent::KeyPress, Qt::Key_F5, Qt::NoModifier);
		QKeyEvent *eventRelease = new QKeyEvent ( QEvent::KeyRelease, Qt::Key_F5, Qt::NoModifier);
		QCoreApplication::postEvent (VisualizationManager::instance().mainScene(), eventPress);
		QCoreApplication::postEvent (VisualizationManager::instance().mainScene(), eventRelease);
	};


	QObject::connect(watcher, &QFileSystemWatcher::fileChanged, onFileChange);

	CHECK_CONDITION(top_level != nullptr);
}

}
