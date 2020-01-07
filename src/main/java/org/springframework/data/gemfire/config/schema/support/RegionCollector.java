/*
 * Copyright 2017-2020 the original author or authors.
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

package org.springframework.data.gemfire.config.schema.support;

import java.util.Set;
import java.util.stream.Collectors;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;

import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.config.schema.SchemaObjectCollector;

/**
 * The {@link RegionCollector} class is an implementation of the {@link SchemaObjectCollector} that is capable of
 * inspecting a context and finding all {@link Region} schema object instances that have been declared in that context.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectCollector
 * @since 2.0.0
 */
public class RegionCollector implements SchemaObjectCollector<Region> {

	@Override
	public Set<Region> collectFrom(ApplicationContext applicationContext) {
		return applicationContext.getBeansOfType(Region.class).values().stream().collect(Collectors.toSet());
	}

	@Override
	public Set<Region> collectFrom(GemFireCache gemfireCache) {
		return gemfireCache.rootRegions().stream().collect(Collectors.toSet());
	}
}
