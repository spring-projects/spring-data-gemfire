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

package org.springframework.data.gemfire.wan;

import com.gemstone.gemfire.cache.util.GatewayHub;

/**
 * The StartupPolicyType class is an enumeration of GemFire GatewayHub Startup Policies.
 *
 * @author John Blum
 * @see com.gemstone.gemfire.cache.util.GatewayHub
 * @since 1.7.0
 */
@SuppressWarnings({"deprecation", "unused" })
public enum StartupPolicyType {
	NONE(GatewayHub.STARTUP_POLICY_NONE),
	PRIMARY(GatewayHub.STARTUP_POLICY_PRIMARY),
	SECONDARY(GatewayHub.STARTUP_POLICY_SECONDARY);

	public static final StartupPolicyType DEFAULT = StartupPolicyType.valueOfIgnoreCase(
		GatewayHub.DEFAULT_STARTUP_POLICY);

	private final String name;

	/**
	 * Constructs an instance of the StartupPolicyType enum initialized with the given GemFire "named",
	 * GatewayHub Startup Policy.
	 *
	 * @param name a String specifying the name used by GemFire for the GatewayHub Startup Policy.
	 */
	StartupPolicyType(final String name) {
		this.name = name;
	}

	/**
	 * Returns a StartupPolicyType enumerated value matching the given official, case-insensitve "name"
	 * for the GatewayHub Startup Policy used by GemFire.
	 *
	 * @param name a String specifying the name used by GemFire for the GatewayHub Startup Policy.
	 * @return a StartupPolicyType enumerated value matching the given name used by GemFire to specify
	 * the GatewayHub Startup Policy.
	 * @see java.lang.String#equalsIgnoreCase(String)
	 * @see #getName()
	 */
	public static StartupPolicyType valueOfIgnoreCase(final String name) {
		for (StartupPolicyType startupPolicyType : values()) {
			if (startupPolicyType.getName().equalsIgnoreCase(name)) {
				return startupPolicyType;
			}
		}

		return null;
	}

	/**
	 * Gets the official name used by GemFire to specify the GatewayHub Startup Policy.
	 *
	 * @return the official GatewayHub Startup Policy name used by GemFire.
	 */
	public String getName() {
		return name;
	}

}
