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

namespace OOModel {

class MetaDefinition;

class EnvisionMetaDefinition {
	public:
		// nodeMacros.h
		static MetaDefinition* attribute();
		static MetaDefinition* private_attribute();

		// typeIdMacros.h
		static MetaDefinition* declare_type_id_common();
		static MetaDefinition* declare_type_id();

		// ItemMacros.h
		static MetaDefinition* item_common_custom_stylename();

		// StandardExpressionVisualizations.h
		static MetaDefinition* begin_standard_expression_visualization_style_h();
		static MetaDefinition* begin_standard_expression_visualization_h();
		static MetaDefinition* begin_standard_enumeration_expression_visualization_h();
		static MetaDefinition* begin_standard_flag_expression_visualization_h();
		static MetaDefinition* expression_part_h();
		static MetaDefinition* prefix_h();
		static MetaDefinition* infix_h();
		static MetaDefinition* infix2_h();
		static MetaDefinition* postfix_h();
		static MetaDefinition* operand_h();
		static MetaDefinition* wrapped_operand_h();

		// StandardExpressionVisualizations.cpp
		static MetaDefinition* begin_standard_expression_visualization_all_cpp();
		static MetaDefinition* begin_standard_expression_visualization_cpp();
		static MetaDefinition* begin_standard_enumeration_expression_visualization_cpp();
		static MetaDefinition* begin_standard_flag_expression_visualization_cpp();
		static MetaDefinition* operand_cpp();
		static MetaDefinition* wrapped_operand_cpp();
		static MetaDefinition* preinpostfix_cpp();
		static MetaDefinition* prefix_cpp();
		static MetaDefinition* infix_cpp();
		static MetaDefinition* infix2_cpp();
		static MetaDefinition* postfix_cpp();

		// StandardExpressionVisualizations merged
		static MetaDefinition* begin_standard_expression_visualization_base();
};

}
