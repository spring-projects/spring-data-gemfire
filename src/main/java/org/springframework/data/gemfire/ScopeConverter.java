/*
 * Copyright 2010-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import org.apache.geode.cache.Scope;
import org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport;

/**
 * The ScopeConverter class is a Spring Converter and JavaBeans PropertyEditor that converts Strings
 * into GemFire Scope constant values.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport
 * @see org.apache.geode.cache.Scope
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public class ScopeConverter extends AbstractPropertyEditorConverterSupport<Scope> {

	/**
	 * Converts the given String source into an instance of GemFire Scope.
	 *
	 * @param source the String to convert into a GemFire Scope.
	 * @return a GemFire Scope for the given String.
	 * @throws java.lang.IllegalArgumentException if the String is not a valid GemFire Scope.
	 * @see org.springframework.data.gemfire.ScopeType#getScope(ScopeType)
	 * @see org.springframework.data.gemfire.ScopeType#valueOfIgnoreCase(String)
	 * @see org.apache.geode.cache.Scope#fromString(String)
	 * @see #assertConverted(String, Object, Class)
	 */
	@Override
	public Scope convert(final String source) {
		try {
			return Scope.fromString(source);
		}
		catch (IllegalArgumentException e) {
			return assertConverted(source, ScopeType.getScope(ScopeType.valueOfIgnoreCase(source)), Scope.class);
		}
	}

}
