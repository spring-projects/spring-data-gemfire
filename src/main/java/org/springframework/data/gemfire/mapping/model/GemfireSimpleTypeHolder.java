/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.mapping.model;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashSet;
import java.util.Set;

import org.springframework.data.mapping.model.SimpleTypeHolder;

/**
 * The GemfireSimpleTypeHolder class is a Spring Data Commons SimpleTypeHolder implementation adding additional
 * simple types to the collection.
 *
 * @author John Blum
 * @see org.springframework.data.mapping.model.SimpleTypeHolder
 * @since 1.6.3
 */
@SuppressWarnings("unused")
public class GemfireSimpleTypeHolder extends SimpleTypeHolder {

	protected static final Set<Class<?>> CUSTOM_SIMPLE_TYPES = new HashSet<Class<?>>(2);

	static {
		CUSTOM_SIMPLE_TYPES.add(BigDecimal.class);
		CUSTOM_SIMPLE_TYPES.add(BigInteger.class);
	}

	/**
	 * Constructs an instance of the GemfireSimpleTypeHolder initialized with a source {@link SimpleTypeHolder}.
	 *
	 * @param source the SimpleTypeHolder used as the source for GemFire's {@link SimpleTypeHolder} implementation.
	 * source must not be {@literal null}.
	 * @throws NullPointerException if source is null.
	 */
	public GemfireSimpleTypeHolder(SimpleTypeHolder source) {
		super(CUSTOM_SIMPLE_TYPES, source);
	}

}
