/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.mapping;

import org.apache.geode.pdx.PdxReader;

import org.springframework.data.mapping.model.PropertyValueProvider;
import org.springframework.util.Assert;

/**
 * {@link PropertyValueProvider} to read property values from {@link PdxReader}.
 *
 * @author Oliver Gierke
 * @author David Turanski
 * @author John Blum
 */
class GemfirePropertyValueProvider implements PropertyValueProvider<GemfirePersistentProperty> {

	private final PdxReader reader;

	/**
	 * Creates a new {@link GemfirePropertyValueProvider} with the given {@link PdxReader}.
	 *
	 * @param reader must not be {@literal null}.
	 */
	public GemfirePropertyValueProvider(PdxReader reader) {
		Assert.notNull(reader, "PdxReader must not be null");
		this.reader = reader;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.convert.PropertyValueProvider#getPropertyValue(org.springframework.data.mapping.PersistentProperty)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <T> T getPropertyValue(GemfirePersistentProperty property) {
		return (T) reader.readField(property.getName());
	}
}
