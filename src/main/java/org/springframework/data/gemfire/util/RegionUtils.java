/*
 * Copyright 2017 the original author or authors.
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

package org.springframework.data.gemfire.util;

import java.util.Optional;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.springframework.util.StringUtils;

/**
 * The {@link RegionUtils} class is an abstract utility class for working with {@link Region Regions}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.RegionAttributes
 * @since 2.0.0
 */
public abstract class RegionUtils extends CacheUtils {

	public static boolean isClient(Region region) {

		return Optional.ofNullable(region)
			.map(Region::getAttributes)
			.map(RegionAttributes::getPoolName)
			.filter(StringUtils::hasText)
			.isPresent();
	}

	/* (non-Javadoc) */
	public static String toRegionPath(String regionName) {
		return String.format("%1$s%2$s", Region.SEPARATOR, regionName);
	}
}
