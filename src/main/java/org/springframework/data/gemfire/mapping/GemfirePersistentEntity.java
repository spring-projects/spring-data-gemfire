/*
 * Copyright 2012 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.mapping;

import org.springframework.data.mapping.PersistentEntity;
import org.springframework.data.mapping.model.BasicPersistentEntity;
import org.springframework.data.util.TypeInformation;
import org.springframework.util.StringUtils;

/**
 * {@link PersistentEntity} implementation adding custom Gemfire related metadata, such as the region the entity is
 * mapped to etc.
 * 
 * @author Oliver Gierke
 */
public class GemfirePersistentEntity<T> extends BasicPersistentEntity<T, GemfirePersistentProperty> {

	private final String regionName;

	/**
	 * Creates a new {@link GemfirePersistentEntity} for the given {@link TypeInformation}.
	 * 
	 * @param information must not be {@literal null}.
	 */
	public GemfirePersistentEntity(TypeInformation<T> information) {

		super(information);

		Class<T> rawType = information.getType();
		Region region = rawType.getAnnotation(Region.class);
		String fallbackName = rawType.getSimpleName();

		this.regionName = region == null || !StringUtils.hasText(region.value()) ? fallbackName : region.value();
	}

	/**
	 * Returns the name of the region the entity shall be stored in.
	 * 
	 * @return
	 */
	public String getRegionName() {
		return this.regionName;
	}
}
