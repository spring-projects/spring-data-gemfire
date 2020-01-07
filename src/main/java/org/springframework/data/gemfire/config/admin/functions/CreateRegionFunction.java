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

package org.springframework.data.gemfire.config.admin.functions;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.gemfire.config.schema.definitions.RegionDefinition;
import org.springframework.data.gemfire.function.annotation.GemfireFunction;

/**
 * The CreateRegionFunction class...
 *
 * @author John Blum
 * @since 1.0.0
 */
public class CreateRegionFunction {

	public static final String CREATE_REGION_FUNCTION_ID = "CreateRegionFunction";

	private final Logger logger = LoggerFactory.getLogger(getClass());

	@GemfireFunction(id = CREATE_REGION_FUNCTION_ID)
	public boolean createRegion(RegionDefinition regionDefinition) {

		Cache gemfireCache = resolveCache();

		if (isNonExistingRegion(gemfireCache, regionDefinition)) {

			RegionFactory regionFactory = gemfireCache.createRegionFactory(regionDefinition.getRegionShortcut());

			Region region = regionFactory.create(regionDefinition.getName());

			if (logger.isInfoEnabled()) {
				logger.info("Created Region [{}] of type [{}]", region.getName(), region.getAttributes().getDataPolicy());
			}

			return true;
		}
		else {

			if (logger.isDebugEnabled()) {
				logger.info("Region with name [{}] already exists", regionDefinition.getName());
			}

			return false;
		}
	}

	protected Cache resolveCache() {
		return CacheFactory.getAnyInstance();
	}

	private boolean isNonExistingRegion(Cache gemfireCache, RegionDefinition regionDefinition) {
		return (gemfireCache.getRegion(regionDefinition.getName()) == null);
	}
}
