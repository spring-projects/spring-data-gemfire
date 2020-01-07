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

package org.springframework.data.gemfire.expiration;

import org.apache.geode.cache.ExpirationAction;

import org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport;

/**
 * The ExpirationActionTypeConverter class is a Spring Converter used to convert a String value into
 * a corresponding ExpirationActionType enumerated value.
 *
 * @author John Blum
 * @see java.beans.PropertyEditorSupport
 * @see org.springframework.core.convert.converter.Converter
 * @see ExpirationActionType
 * @since 1.6.0
 */
public class ExpirationActionConverter extends AbstractPropertyEditorConverterSupport<ExpirationAction> {

	/**
	 * Converts the given String into an appropriate Pivotal GemFire ExpirationAction.
	 *
	 * @param source the String to convert into an Pivotal GemFire ExpirationAction.
	 * @return an Pivotal GemFire ExpirationAction value for the given String.
	 * @throws java.lang.IllegalArgumentException if the String is not a valid Pivotal GemFire ExpirationAction.
	 * @see ExpirationActionType#valueOfIgnoreCase(String)
	 * @see org.apache.geode.cache.ExpirationAction
	 */
	@Override
	public ExpirationAction convert(final String source) {
		return assertConverted(source, ExpirationActionType.getExpirationAction(
			ExpirationActionType.valueOfIgnoreCase(source)), ExpirationAction.class);
	}

}
