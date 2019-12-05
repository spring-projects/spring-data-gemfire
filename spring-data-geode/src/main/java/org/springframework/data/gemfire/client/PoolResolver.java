/*
 * Copyright 2019 the original author or authors.
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
package org.springframework.data.gemfire.client;

import java.util.Optional;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.client.Pool;

import org.springframework.lang.Nullable;
import org.springframework.util.StringUtils;

/**
 * {@link PoolResolver} is a strategy interface for resolving references to Apache Geode {@link Pool} instances.
 *
 * This is used throughout SDG's codebase to separate SDG's {@link Pool} resolution logic from being explicitly tied to
 * to Apache Geode's static {@link org.apache.geode.cache.client.PoolManager} class.  This interfaces also serves
 * as an SPI for different strategies when resolving a {@link Pool}.
 *
 * @author John Blum
 * @see java.lang.FunctionalInterface
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.Pool
 * @since 2.3.0
 */
@FunctionalInterface
public interface PoolResolver {

	/**
	 * Resolves the {@link Pool} instance used by the given {@link Region}.
	 *
	 * If the {@link Region} is a {@literal client} {@link Region} but does not explicitly configure
	 * a specific {@link Pool} reference, then the {@literal DEFAULT} {@link Pool} is returned.
	 *
	 * If the {@link Region} is {@literal local} or a {@literal peer} {@link Region}, then {@literal null}
	 * is returned.
	 *
 	 * @param region {@link Region} from which to resolve the associated {@link Pool}.
	 * @return the {@link Pool} instance associated with the given {@link Region},
	 * or the {@literal DEFAULT} {@link Pool} if the {@link Region} is a {@literal client} {@link Region},
	 * or {@literal null} if the {@link Region} is not a {@literal client} {@link Region}.
	 * @see org.apache.geode.cache.Region
	 * @see org.apache.geode.cache.client.Pool
	 */
	default @Nullable Pool resolve(@Nullable Region<?, ?> region) {

		return Optional.ofNullable(region)
			.map(Region::getAttributes)
			.map(RegionAttributes::getPoolName)
			.filter(StringUtils::hasText)
			.map(this::resolve)
			.orElse(null);
	}

	/**
	 * Resolves a {@link Pool} with the given {@link String name}.
	 *
	 * @param poolName {@link String name} of the {@link Pool} to resolve.
	 * @return the {@link Pool} with the given {@link String name} or {@link null} if no {@link Pool} exists with
	 * the {@link String name}.
	 * @see org.apache.geode.cache.client.Pool
	 */
	@Nullable Pool resolve(@Nullable String poolName);

}
