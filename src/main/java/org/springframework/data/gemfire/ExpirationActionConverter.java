/*
 * Copyright 2010-2019 the original author or authors.
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

import org.springframework.data.gemfire.support.AbstractPropertyEditorConverterSupport;

import com.gemstone.gemfire.cache.ExpirationAction;

/**
 * The ExpirationActionTypeConverter class is a Spring Converter used to convert a String value into
 * a corresponding ExpirationActionType enumerated value.
 *
 * @author John Blum
 * @see java.beans.PropertyEditorSupport
 * @see org.springframework.core.convert.converter.Converter
 * @see org.springframework.data.gemfire.ExpirationActionType
 * @since 1.6.0
 */
public class ExpirationActionConverter extends AbstractPropertyEditorConverterSupport<ExpirationAction> {

	/**
	 * Converts the given String into an appropriate GemFire ExpirationAction.
	 *
	 * @param source the String to convert into an GemFire ExpirationAction.
	 * @return an GemFire ExpirationAction value for the given String.
	 * @throws java.lang.IllegalArgumentException if the String is not a valid GemFire ExpirationAction.
	 * @see org.springframework.data.gemfire.ExpirationActionType#valueOfIgnoreCase(String)
	 * @see com.gemstone.gemfire.cache.ExpirationAction
	 */
	@Override
	public ExpirationAction convert(final String source) {
		return assertConverted(source, ExpirationActionType.getExpirationAction(
			ExpirationActionType.valueOfIgnoreCase(source)), ExpirationAction.class);
	}

}
