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

import org.springframework.data.mapping.context.MappingContext;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Region;

/**
 * Simple value object to abstract access to regions by name and mapped type.
 * 
 * @author Oliver Gierke
 */
public class Regions implements Iterable<Region<?, ?>> {

	private final Map<String, Region<?, ?>> regions;

	private final MappingContext<? extends GemfirePersistentEntity<?>, ?> context;

	/**
	 * Creates a new {@link Regions} wrapper for the given {@link Region}s and
	 * {@link MappingContext}.
	 * 
	 * @param regions must not be {@literal null}.
	 * @param context must not be {@literal null}.
	 */
	public Regions(Iterable<Region<?, ?>> regions, MappingContext<? extends GemfirePersistentEntity<?>, ?> context) {

		Assert.notNull(regions);
		Assert.notNull(context);

		Map<String, com.gemstone.gemfire.cache.Region<?, ?>> regionMap = new HashMap<String, Region<?, ?>>();

		for (Region<?, ?> region : regions) {
			regionMap.put(region.getName(), region);
		}

		this.regions = Collections.unmodifiableMap(regionMap);
		this.context = context;
	}

	/**
	 * Returns the {@link Region} the given type is mapped to. Will try to find
	 * a {@link Region} with the simple class name in case no mapping
	 * information is found.
	 * 
	 * @param type must not be {@literal null}.
	 * @return the {@link Region} the given type is mapped to.
	 */
	@SuppressWarnings("unchecked")
	public <T> Region<?, T> getRegion(Class<T> type) {

		Assert.notNull(type);

		GemfirePersistentEntity<?> entity = context.getPersistentEntity(type);
		return (Region<?, T>) (entity == null ? regions.get(type.getSimpleName()) : regions.get(entity.getRegionName()));
	}

	/**
	 * Returns the {@link Region} with the given name.
	 * 
	 * @param name must not be {@literal null}.
	 * @return the {@link Region} with the given name.
	 */
	@SuppressWarnings("unchecked")
	public <S, T> Region<S, T> getRegion(String name) {

		Assert.notNull(name);

		return (Region<S, T>) regions.get(name);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Iterable#iterator()
	 */
	@Override
	public Iterator<Region<?, ?>> iterator() {
		return regions.values().iterator();
	}
}
