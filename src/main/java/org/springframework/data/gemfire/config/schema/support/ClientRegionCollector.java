/*
 * Copyright 2017-2019 the original author or authors.
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

import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeSet;

import java.util.Set;
import java.util.stream.Collectors;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.util.RegionUtils;

/**
 * The {@link ClientRegionCollector} class is an extension of the {@link RegionCollector} which applies additional
 * filtering to find only client {@link Region Regions} in a given context.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.data.gemfire.config.schema.SchemaObjectCollector
 * @since 2.0.0
 */
public class ClientRegionCollector extends RegionCollector {

	@Override
	public Set<Region> collectFrom(ApplicationContext applicationContext) {
		return onlyClientRegions(super.collectFrom(applicationContext));
	}

	@Override
	public Set<Region> collectFrom(GemFireCache gemfireCache) {
		return onlyClientRegions(super.collectFrom(gemfireCache));
	}

	private Set<Region> onlyClientRegions(Set<Region> regions) {
		return nullSafeSet(regions).stream().filter(RegionUtils::isClient).collect(Collectors.toSet());
	}
}
