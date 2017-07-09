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

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;

import org.apache.geode.cache.Region;
import org.springframework.data.mapping.context.MappingContext;
import org.springframework.util.Assert;

/**
 * Simple value object to abstract access to regions by name and mapped type.
 *
 * @author Oliver Gierke
 * @author John Blum
 */
public class Regions implements Iterable<Region<?, ?>> {

	private final Map<String, Region<?, ?>> regions;

	private final MappingContext<? extends GemfirePersistentEntity<?>, ?> mappingContext;

	/**
	 * Creates a new {@link Regions} wrapper for the given {@link Region}s and
	 * {@link MappingContext}.
	 *
	 * @param regions must not be {@literal null}.
	 * @param mappingContext must not be {@literal null}.
	 */
	public Regions(Iterable<Region<?, ?>> regions,
			MappingContext<? extends GemfirePersistentEntity<?>, ?> mappingContext) {

		Assert.notNull(regions, "Regions must not be null");
		Assert.notNull(mappingContext, "MappingContext must not be null");

		Map<String, Region<?, ?>> regionMap = new HashMap<>();

		for (Region<?, ?> region : regions) {
			regionMap.put(region.getName(), region);
			regionMap.put(region.getFullPath(), region);
		}

		this.regions = Collections.unmodifiableMap(regionMap);
		this.mappingContext = mappingContext;
	}

	/**
	 * Returns the {@link Region} the given type is mapped to. Will try to find
	 * a {@link Region} with the simple class name in case no mapping
	 * information is found.
	 *
	 * @param <T> the Region value class type.
	 * @param entityType must not be {@literal null}.
	 * @return the {@link Region} the given type is mapped to.
	 */
	@SuppressWarnings("unchecked")
	public <T> Region<?, T> getRegion(Class<T> entityType) {

		Assert.notNull(entityType, "Entity type must not be null");

		String regionName = Optional.ofNullable(this.mappingContext.getPersistentEntity(entityType))
			.map(entity -> entity.getRegionName()).orElseGet(entityType::getSimpleName);

		return (Region<?, T>) this.regions.get(regionName);
	}

	/**
	 * Returns the {@link Region} with the given name or path.
	 *
	 * @param <S> the Region key class type.
	 * @param <T> the Region value class type.
	 * @param namePath must not be {@literal null}, and either identifies the Region by name or the fully-qualified path.
	 * @return the {@link Region} with the given name or path.
	 */
	@SuppressWarnings("unchecked")
	public <S, T> Region<S, T> getRegion(String namePath) {

		Assert.hasText(namePath, "Region name/path is required");

		return (Region<S, T>) this.regions.get(namePath);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Iterable#iterator()
	 */
	@Override
	public Iterator<Region<?, ?>> iterator() {
		return this.regions.values().iterator();
	}
}
