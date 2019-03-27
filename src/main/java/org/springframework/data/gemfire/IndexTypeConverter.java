/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport;
import org.springframework.util.StringUtils;

/**
 * The IndexTypeConverter class is a Spring Converter implementation as well as a JavaBeans PropertyEditor
 * that converts a given String value into a proper IndexType.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.IndexType
 * @see org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport
 * @since 1.5.2
 */
@SuppressWarnings("unused")
public class IndexTypeConverter extends AbstractPropertyEditorConverterSupport<IndexType> {

	/**
	 * Converts the given String value into an appropriate IndexType.
	 *
	 * @param value the String to convert into an appropriate IndexType enumerated value.
	 * @return an IndexType converted from the given String.
	 * @throws java.lang.IllegalArgumentException if the given String could not be converted into
	 * an appropriate IndexType enumerated value.
	 * @see #assertConverted(String, Object, Class)
	 * @see org.springframework.data.gemfire.IndexType#valueOfIgnoreCase(String)
	 * @see org.springframework.util.StringUtils#trimWhitespace(String)
	 */
	@Override
	public IndexType convert(final String value) {
		return assertConverted(value, IndexType.valueOfIgnoreCase(StringUtils.trimWhitespace(value)), IndexType.class);
	}

}
