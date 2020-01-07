/*
 * Copyright 2010-2020 the original author or authors.
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

package org.springframework.data.gemfire.eviction;

import org.apache.geode.cache.EvictionAction;

import org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport;

/**
 * The EvictionActionConverter class is a Spring Converter and JavaBeans PropertyEditor that converts
 * an Object value into an instance of Pivotal GemFire EvictionAction.
 *
 * @author John Blum
 * @see EvictionActionType
 * @see org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport
 * @see org.apache.geode.cache.EvictionAction
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public class EvictionActionConverter extends AbstractPropertyEditorConverterSupport<EvictionAction> {

	/**
	 * Converts the given String into a Pivotal GemFire EvictionAction value.
	 *
	 * @param source the String to convert.
	 * @return the Pivotal GemFire EvictionAction value matching the given String.
	 * @throws java.lang.IllegalArgumentException if the String could not be converted into
	 * an instance of Pivotal GemFire EvictionAction.
	 * @see org.apache.geode.cache.EvictionAction
	 */
	@Override
	public EvictionAction convert(final String source) {
		return assertConverted(source, EvictionActionType.getEvictionAction(
			EvictionActionType.valueOfIgnoreCase(source)), EvictionAction.class);
	}

}
