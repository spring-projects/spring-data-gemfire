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

package org.springframework.data.gemfire;

import java.util.concurrent.ConcurrentMap;

import org.springframework.data.gemfire.util.DistributedSystemUtils;
import org.springframework.util.ClassUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheClosedException;
import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.distributed.DistributedSystem;
import com.gemstone.gemfire.management.internal.cli.util.spring.StringUtils;

/**
 * GemfireUtils is an abstract utility class encapsulating common functionality to access features and capabilities
 * of GemFire based on version and other configuration meta-data.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.util.DistributedSystemUtils
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.CacheFactory
 * @see com.gemstone.gemfire.cache.Region
 * @see com.gemstone.gemfire.cache.client.ClientCache
 * @see com.gemstone.gemfire.cache.client.ClientCacheFactory
 * @see com.gemstone.gemfire.distributed.DistributedSystem
 * @since 1.3.3
 */
@SuppressWarnings("unused")
public abstract class GemfireUtils extends DistributedSystemUtils {

	public final static String GEMFIRE_VERSION = CacheFactory.getVersion();

	public static boolean isDurable(ClientCache clientCache) {
		DistributedSystem distributedSystem = getDistributedSystem(clientCache);

		// NOTE technically the following code snippet would be more useful/valuable but is not "testable"!
		//((InternalDistributedSystem) distributedSystem).getConfig().getDurableClientId();

		return (isConnected(distributedSystem) && StringUtils.hasText(distributedSystem.getProperties()
			.getProperty(DURABLE_CLIENT_ID_PROPERTY_NAME, null)));
	}

	public static boolean closeCache() {
		try {
			CacheFactory.getAnyInstance().close();
			return true;
		}
		catch (Exception ignore) {
			return false;
		}
	}

	public static boolean closeClientCache() {
		try {
			ClientCacheFactory.getAnyInstance().close();
			return true;
		}
		catch (Exception ignore) {
			return false;
		}
	}

	public static Cache getCache() {
		try {
			return CacheFactory.getAnyInstance();
		}
		catch (CacheClosedException ignore) {
			return null;
		}
	}

	public static ClientCache getClientCache() {
		try {
			return ClientCacheFactory.getAnyInstance();
		}
		catch (CacheClosedException ignore) {
			return null;
		}
	}

	public static boolean isGemfireVersionGreaterThanEqualTo(double expectedVersion) {
		double actualVersion = Double.parseDouble(GEMFIRE_VERSION.substring(0, 3));
		return actualVersion >= expectedVersion;
	}

	public static boolean isGemfireVersion65OrAbove() {
		// expected 'major.minor'
		try {
			double version = Double.parseDouble(GEMFIRE_VERSION.substring(0, 3));
			return version >= 6.5;
		}
		catch (NumberFormatException e) {
			// NOTE based on logic from the PartitionedRegionFactoryBean class...
			return ConcurrentMap.class.isAssignableFrom(Region.class);
		}
	}

	public static boolean isGemfireVersion7OrAbove() {
		try {
			return isGemfireVersionGreaterThanEqualTo(7.0);
		}
		catch (NumberFormatException e) {
			// NOTE the com.gemstone.gemfire.distributed.ServerLauncher class only exists in GemFire v 7.0.x or above...
			return ClassUtils.isPresent("com.gemstone.gemfire.distributed.ServerLauncher",
				Thread.currentThread().getContextClassLoader());
		}
	}

	public static boolean isGemfireVersion8OrAbove() {
		try {
			return isGemfireVersionGreaterThanEqualTo(8.0);
		}
		catch (NumberFormatException e) {
			// NOTE the com.gemstone.gemfire.management.internal.web.domain.LinkIndex class only exists
			// in GemFire v 8.0.0 or above...
			return ClassUtils.isPresent("com.gemstone.gemfire.management.internal.web.domain.LinkIndex",
				Thread.currentThread().getContextClassLoader());
		}
	}

	public static void main(final String... args) {
		System.out.printf("GemFire Version %1$s%n", GEMFIRE_VERSION);
		//System.out.printf("Is GemFire Version 6.5 of Above? %1$s%n", isGemfireVersion65OrAbove());
		//System.out.printf("Is GemFire Version 7.0 of Above? %1$s%n", isGemfireVersion7OrAbove());
	}

}
