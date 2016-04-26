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
import org.springframework.data.gemfire.util.CacheUtils;
import org.springframework.util.ClassUtils;
import org.w3c.dom.Element;

import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.internal.GemFireVersion;

/**
 * GemfireUtils is an abstract utility class encapsulating common functionality to access features and capabilities
 * of GemFire based on version and other configuration meta-data.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.util.DistributedSystemUtils
 * @see com.gemstone.gemfire.cache.CacheFactory
 * @see com.gemstone.gemfire.cache.Region
 * @since 1.3.3
 */
@SuppressWarnings("unused")
public abstract class GemfireUtils extends CacheUtils {

	public final static String GEMFIRE_NAME = GemFireVersion.getProductName();
	public final static String GEMFIRE_VERSION = CacheFactory.getVersion();

	private static final String ASYNC_EVENT_QUEUE_ELEMENT_NAME = "async-event-queue";
	private static final String ASYNC_EVENT_QUEUE_TYPE_NAME = "com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue";
	private static final String CQ_ELEMENT_NAME = "cq-listener-container";
	private static final String CQ_TYPE_NAME = "com.gemstone.gemfire.cache.query.internal.cq.CqServiceFactoryImpl";
	private static final String GATEWAY_RECEIVER_ELEMENT_NAME = "gateway-receiver";
	private static final String GATEWAY_RECEIVER_TYPE_NAME = "com.gemstone.gemfire.internal.cache.wan.GatewayReceiverFactoryImpl";
	private static final String GATEWAY_SENDER_ELEMENT_NAME = "gateway-sender";
	private static final String GATEWAY_SENDER_TYPE_NAME = "com.gemstone.gemfire.internal.cache.wan.GatewaySenderFactoryImpl";

	/* (non-Javadoc) */
	public static boolean isClassAvailable(String fullyQualifiedClassName) {
		return ClassUtils.isPresent(fullyQualifiedClassName, GemfireUtils.class.getClassLoader());
	}

	/* (non-Javadoc) */
	public static boolean isGemfireFeatureAvailable(GemfireFeature feature) {
		boolean featureAvailable = (!GemfireFeature.AEQ.equals(feature) || isAsyncEventQueueAvailable());
		featureAvailable &= (!GemfireFeature.CONTINUOUS_QUERY.equals(feature) || isContinuousQueryAvailable());
		featureAvailable &= (!GemfireFeature.WAN.equals(feature) || isGatewayAvailable());
		return featureAvailable;
	}

	/* (non-Javadoc) */
	public static boolean isGemfireFeatureAvailable(Element element) {
		boolean featureAvailable = (!isAsyncEventQueue(element) || isAsyncEventQueueAvailable());
		featureAvailable &= (!isContinuousQuery(element) || isContinuousQueryAvailable());
		featureAvailable &= (!isGateway(element) || isGatewayAvailable());
		return featureAvailable;
	}

	/* (non-Javadoc) */
	public static boolean isGemfireFeatureUnavailable(GemfireFeature feature) {
		return !isGemfireFeatureAvailable(feature);
	}

	/* (non-Javadoc) */
	public static boolean isGemfireFeatureUnavailable(Element element) {
		return !isGemfireFeatureAvailable(element);
	}

	/* (non-Javadoc) */
	private static boolean isAsyncEventQueue(Element element) {
		return ASYNC_EVENT_QUEUE_ELEMENT_NAME.equals(element.getLocalName());
	}

	/* (non-Javadoc) */
	private static boolean isAsyncEventQueueAvailable() {
		return isClassAvailable(ASYNC_EVENT_QUEUE_TYPE_NAME);
	}

	/* (non-Javadoc) */
	private static boolean isContinuousQuery(Element element) {
		return CQ_ELEMENT_NAME.equals(element.getLocalName());
	}

	/* (non-Javadoc) */
	private static boolean isContinuousQueryAvailable() {
		return isClassAvailable(CQ_TYPE_NAME);
	}

	/* (non-Javadoc) */
	private static boolean isGateway(Element element) {
		String elementLocalName = element.getLocalName();
		return (GATEWAY_RECEIVER_ELEMENT_NAME.equals(elementLocalName)
			|| GATEWAY_SENDER_ELEMENT_NAME.equals(elementLocalName));
	}

	/* (non-Javadoc) */
	private static boolean isGatewayAvailable() {
		return isClassAvailable(GATEWAY_SENDER_TYPE_NAME);
	}

	/* (non-Javadoc) */
	public static boolean isGemfireVersionGreaterThanEqualTo(double expectedVersion) {
		double actualVersion = Double.parseDouble(GEMFIRE_VERSION.substring(0, 3));
		return (actualVersion >= expectedVersion);
	}

	/* (non-Javadoc) */
	public static boolean isGemfireVersion65OrAbove() {
		try {
			return isGemfireVersionGreaterThanEqualTo(6.5);
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
			// NOTE the com.gemstone.gemfire.distributed.ServerLauncher class only exists in GemFire v 7.0.x or above...
			return ClassUtils.isPresent("com.gemstone.gemfire.distributed.ServerLauncher",
				Thread.currentThread().getContextClassLoader());
		}
	}

	/* (non-Javadoc) */
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
		System.out.printf("GemFire Product Name (%1$s) Version (%2$s)%n", GEMFIRE_NAME, GEMFIRE_VERSION);
		//System.out.printf("Is GemFire Version 6.5 of Above? %1$s%n", isGemfireVersion65OrAbove());
		//System.out.printf("Is GemFire Version 7.0 of Above? %1$s%n", isGemfireVersion7OrAbove());
	}

}
