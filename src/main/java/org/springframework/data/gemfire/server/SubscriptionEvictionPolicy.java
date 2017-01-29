/*
 * Copyright 2011-2018 the original author or authors.
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

package org.springframework.data.gemfire.server;

import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.server.ClientSubscriptionConfig;

/**
 * Enumeration of the various client subscription policies for {@link CacheServer}.
 *
 * @author Costin Leau
 * @author John Blum
 * @since 1.1.0
 */
public enum SubscriptionEvictionPolicy {
	ENTRY,
	MEM,
	NONE;

	public static final SubscriptionEvictionPolicy DEFAULT = SubscriptionEvictionPolicy.valueOfIgnoreCase(
		ClientSubscriptionConfig.DEFAULT_EVICTION_POLICY);

	/**
	 * Returns the value of the given String name as a SubscriptionEvictionPolicy enum using a case-insensitive,
	 * equality comparison.
	 *
	 * @param name the String name of a SubscriptionEvictionPolicy enumerated value.
	 * @return a SubscriptionEvictionPolicy enumerated value given a String name or null if no enum value
	 * with name was found.
	 * @see org.springframework.data.gemfire.server.SubscriptionEvictionPolicy
	 * @see java.lang.String#equalsIgnoreCase(String)
	 * @see #values()
	 * @see #name()
	 */
	public static SubscriptionEvictionPolicy valueOfIgnoreCase(final String name) {
		for (SubscriptionEvictionPolicy subscriptionEvictionPolicy : values()) {
			if (subscriptionEvictionPolicy.name().equalsIgnoreCase(name)) {
				return subscriptionEvictionPolicy;
			}
		}

		return null;
	}

	/**
	 * Null-safe utility method for setting the client's subscription eviction policy on the configuration meta-data.
	 *
	 * @param config a GemFire ClientSubscriptionConfig object holding the configuration setting and meta-data
	 * about the client's subscription configuration.
	 * @return the ClientSubscriptionConfig object.
	 * @see org.apache.geode.cache.server.ClientSubscriptionConfig#setEvictionPolicy(String)
	 */
	public ClientSubscriptionConfig setEvictionPolicy(final ClientSubscriptionConfig config) {
		if (config != null) {
			config.setEvictionPolicy(name().toLowerCase());
		}

		return config;
	}

}
