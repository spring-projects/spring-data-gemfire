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

package org.springframework.data.gemfire.util;

import java.util.Properties;

import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.server.CacheServer;
import com.gemstone.gemfire.distributed.DistributedSystem;
import com.gemstone.gemfire.distributed.Locator;
import com.gemstone.gemfire.distributed.internal.DistributionConfig;
import com.gemstone.gemfire.distributed.internal.InternalDistributedSystem;
import com.gemstone.gemfire.distributed.internal.InternalLocator;
import com.gemstone.gemfire.internal.DistributionLocator;

import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * DistributedSystemUtils is an abstract utility class for working with the GemFire DistributedSystem.
 *
 * @author John Blum
 * @see com.gemstone.gemfire.cache.GemFireCache
 * @see com.gemstone.gemfire.distributed.DistributedSystem
 * @see com.gemstone.gemfire.distributed.internal.DistributionConfig
 * @see com.gemstone.gemfire.distributed.internal.InternalDistributedSystem
 * @see com.gemstone.gemfire.internal.DistributionLocator
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public abstract class DistributedSystemUtils extends SpringUtils {

	public static final int DEFAULT_CACHE_SERVER_PORT = CacheServer.DEFAULT_PORT;
	public static final int DEFAULT_LOCATOR_PORT = DistributionLocator.DEFAULT_LOCATOR_PORT;

	public static final String DURABLE_CLIENT_ID_PROPERTY_NAME = DistributionConfig.DURABLE_CLIENT_ID_NAME;
	public static final String DURABLE_CLIENT_TIMEOUT_PROPERTY_NAME = DistributionConfig.DURABLE_CLIENT_TIMEOUT_NAME;
	public static final String GEMFIRE_PREFIX = DistributionConfig.GEMFIRE_PREFIX;
	public static final String NAME_PROPERTY_NAME = DistributionConfig.NAME_NAME;

	/* (non-Javadoc) */
	public static Properties configureDurableClient(Properties gemfireProperties,
			String durableClientId, Integer durableClientTimeout) {

		if (StringUtils.hasText(durableClientId)) {
			Assert.notNull(gemfireProperties, "gemfireProperties must not be null");

			gemfireProperties.setProperty(DURABLE_CLIENT_ID_PROPERTY_NAME, durableClientId);

			if (durableClientTimeout != null) {
				gemfireProperties.setProperty(DURABLE_CLIENT_TIMEOUT_PROPERTY_NAME, durableClientTimeout.toString());
			}
		}

		return gemfireProperties;
	}

	/* (non-Javadoc) */
	public static boolean isConnected(DistributedSystem distributedSystem) {
		return (distributedSystem != null && distributedSystem.isConnected());
	}

	/* (non-Javadoc) */
	public static boolean isNotConnected(DistributedSystem distributedSystem) {
		return !isConnected(distributedSystem);
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	public static <T extends DistributedSystem> T getDistributedSystem() {
		return (T) InternalDistributedSystem.getAnyInstance();
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	public static <T extends DistributedSystem> T getDistributedSystem(GemFireCache gemfireCache) {
		return (gemfireCache != null ? (T) gemfireCache.getDistributedSystem() : null);
	}

	/* (non-Javadoc)*/
	@SuppressWarnings("unchecked")
	public static <T extends Locator> T getLocator() {
		return (T) InternalLocator.getLocator();
	}
}
