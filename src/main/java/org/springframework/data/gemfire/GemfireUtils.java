/*
 * Copyright 2010-2018 the original author or authors.
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

import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.Region;
import org.apache.geode.internal.GemFireVersion;
import org.springframework.data.gemfire.util.RegionUtils;
import org.springframework.util.ClassUtils;

/**
 * {@link GemfireUtils} is an abstract utility class encapsulating common functionality to access features
 * and capabilities of GemFire based on version and other configuration meta-data.
 *
 * @author John Blum
 * @see org.apache.geode.cache.CacheFactory
 * @see org.apache.geode.cache.Region
 * @see org.springframework.data.gemfire.util.RegionUtils
 * @since 1.3.3
 */
@SuppressWarnings("unused")
public abstract class GemfireUtils extends RegionUtils {

	public final static String APACHE_GEODE_NAME = "Apache Geode";
	public final static String GEMFIRE_NAME = apacheGeodeProductName();
	public final static String GEMFIRE_VERSION = apacheGeodeVersion();
	public final static String UNKNOWN = "unknown";

	/* (non-Javadoc) */
	public static String apacheGeodeProductName() {

		try {
			return GemFireVersion.getProductName();
		}
		catch (Throwable ignore) {
			return APACHE_GEODE_NAME;
		}
	}

	/* (non-Javadoc) */
	public static String apacheGeodeVersion() {

		try {
			return CacheFactory.getVersion();
		}
		catch (Throwable ignore) {
			return UNKNOWN;
		}
	}

	/* (non-Javadoc) */
	public static boolean isGemfireVersionGreaterThanEqualTo(double expectedVersion) {
		double actualVersion = Double.parseDouble(GEMFIRE_VERSION.substring(0, 3));
		return actualVersion >= expectedVersion;
	}

	/* (non-Javadoc) */
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

	/* (non-Javadoc) */
	public static boolean isGemfireVersion7OrAbove() {
		try {
			return isGemfireVersionGreaterThanEqualTo(7.0);
		}
		catch (NumberFormatException e) {
			// NOTE the org.apache.geode.distributed.ServerLauncher class only exists in GemFire v 7.0.x or above...
			return ClassUtils.isPresent("org.apache.geode.distributed.ServerLauncher",
				Thread.currentThread().getContextClassLoader());
		}
	}

	/* (non-Javadoc) */
	public static boolean isGemfireVersion8OrAbove() {

		try {
			return isGemfireVersionGreaterThanEqualTo(8.0);
		}
		catch (NumberFormatException e) {
			// NOTE the org.apache.geode.management.internal.web.domain.LinkIndex class only exists
			// in GemFire v 8.0.0 or above...
			return ClassUtils.isPresent("org.apache.geode.management.internal.web.domain.LinkIndex",
				Thread.currentThread().getContextClassLoader());
		}
	}

	public static void main(final String... args) {
		System.out.printf("GemFire Version %1$s%n", GEMFIRE_VERSION);
		//System.out.printf("Is GemFire Version 6.5 of Above? %1$s%n", isGemfireVersion65OrAbove());
		//System.out.printf("Is GemFire Version 7.0 of Above? %1$s%n", isGemfireVersion7OrAbove());
	}
}
