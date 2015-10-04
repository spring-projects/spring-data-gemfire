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

import org.springframework.data.gemfire.config.support.GemfireFeature;
import org.springframework.data.gemfire.util.DistributedSystemUtils;
import org.springframework.util.ClassUtils;
import org.w3c.dom.Element;

import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.internal.GemFireVersion;

/**
 * GemfireUtils is an abstract utility class encapsulating common functionality to access features and capabilities
 * of GemFire based on version and other configuration meta-data.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.util.DistributedSystemUtils
 * @since 1.3.3
 */
@SuppressWarnings("unused")
public abstract class GemfireUtils extends DistributedSystemUtils {

	public final static String GEMFIRE_NAME = GemFireVersion.getProductName();
	public final static String GEMFIRE_VERSION = CacheFactory.getVersion();

	// TODO use a more reliable means of determining implementing class for GemFire features such as Java's SPI support
	private static final String ASYNC_EVENT_QUEUE_TYPE_NAME = "com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue";
	private static final String CQ_TYPE_NAME = "com.gemstone.gemfire.cache.query.internal.cq.CqServiceFactoryImpl";
	private static final String GATEWAY_TYPE_NAME = "com.gemstone.gemfire.internal.cache.wan.GatewaySenderFactoryImpl";

	public static boolean isClassAvailable(String fullyQualifiedClassName) {
		return ClassUtils.isPresent(fullyQualifiedClassName, GemfireUtils.class.getClassLoader());
	}

	public static boolean isGemfireFeatureAvailable(GemfireFeature feature) {
		boolean featureAvailable = (!GemfireFeature.AEQ.equals(feature) || isAsyncEventQueueAvailable());
		featureAvailable &= (!GemfireFeature.CONTINUOUS_QUERY.equals(feature) || isContinuousQueryAvailable());
		featureAvailable &= (!GemfireFeature.WAN.equals(feature) || isGatewayAvailable());
		return featureAvailable;
	}

	public static boolean isGemfireFeatureAvailable(Element element) {
		boolean featureAvailable = (!isAsyncEventQueue(element) || isAsyncEventQueueAvailable());
		featureAvailable &= (!isContinuousQuery(element) || isContinuousQueryAvailable());
		featureAvailable &= (!isGateway(element) || isGatewayAvailable());
		return featureAvailable;
	}

	public static boolean isGemfireFeatureUnavailable(GemfireFeature feature) {
		return !isGemfireFeatureAvailable(feature);
	}

	public static boolean isGemfireFeatureUnavailable(Element element) {
		return !isGemfireFeatureAvailable(element);
	}

	private static boolean isAsyncEventQueue(Element element) {
		return "async-event-queue".equals(element.getLocalName());
	}

	private static boolean isAsyncEventQueueAvailable() {
		return isClassAvailable(ASYNC_EVENT_QUEUE_TYPE_NAME);
	}

	private static boolean isContinuousQuery(Element element) {
		return "cq-listener-container".equals(element.getLocalName());
	}

	private static boolean isContinuousQueryAvailable() {
		return isClassAvailable(CQ_TYPE_NAME);
	}

	private static boolean isGateway(Element element) {
		String elementLocalName = element.getLocalName();
		return ("gateway-receiver".equals(elementLocalName) || "gateway-sender".equals(elementLocalName));
	}

	private static boolean isGatewayAvailable() {
		return isClassAvailable(GATEWAY_TYPE_NAME);
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

	public static boolean isGemfireVersionGreaterThanEqual(double expectedVersion) {
		double actualVersion = Double.parseDouble(GEMFIRE_VERSION.substring(0, 3));
		return (actualVersion >= expectedVersion);
	}

	public static boolean isGemfireVersion65OrAbove() {
		try {
			return isGemfireVersionGreaterThanEqual(6.5);
		}
		catch (NumberFormatException e) {
			// NOTE based on logic from the PartitionedRegionFactoryBean class...
			return ConcurrentMap.class.isAssignableFrom(Region.class);
		}
	}

	public static boolean isGemfireVersion7OrAbove() {
		try {
			return isGemfireVersionGreaterThanEqual(7.0);
		}
		catch (NumberFormatException e) {
			// NOTE the com.gemstone.gemfire.distributed.ServerLauncher class only exists in GemFire v 7.0.x or above...
			return ClassUtils.isPresent("com.gemstone.gemfire.distributed.ServerLauncher",
				Thread.currentThread().getContextClassLoader());
		}
	}

	public static boolean isGemfireVersion8OrAbove() {
		try {
			return isGemfireVersionGreaterThanEqual(8.0);
		}
		catch (NumberFormatException e) {
			// NOTE the com.gemstone.gemfire.management.internal.web.domain.LinkIndex class only exists
			// in GemFire v 8.0.0 or above...
			return ClassUtils.isPresent("com.gemstone.gemfire.management.internal.web.domain.LinkIndex",
				Thread.currentThread().getContextClassLoader());
		}
	}

	public static void main(final String... args) {
		System.out.printf("GemFire Product Name (%1$s) Version (%2$s)%n", GEMFIRE_NAME, GEMFIRE_VERSION);
		//System.out.printf("Is GemFire Version 6.5 of Above? %1$s%n", isGemfireVersion65OrAbove());
		//System.out.printf("Is GemFire Version 7.0 of Above? %1$s%n", isGemfireVersion7OrAbove());
	}

}
