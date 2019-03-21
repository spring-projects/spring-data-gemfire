/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * The JndiDataSourceType class is an enumeration of valid JNDI DataSource implementation types supported by GemFire.
 *
 * @author John Blum
 * @link http://gemfire.docs.pivotal.io/latest/userguide/index.html#reference/topics/cache_xml.html#jndi-binding
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public enum JndiDataSourceType {
	MANAGED("ManagedDataSource"),
	POOLED("PooledDataSource"),
	SIMPLE("SimpleDataSource"),
	XA("XAPooledDataSource");

	private final String name;

	/**
	 * Constructs an instance of the JndiDataSourceType enum initialized with the specified name used by GemFire to
	 * specify supported JNDI DataSource implementations.
	 *
	 * @param name the GemFire named JNDI DataSource implementation.
	 */
	JndiDataSourceType(final String name) {
		Assert.hasText(name, "The JNDI DataSource Type 'name' must be specified!");
		this.name = name;
	}

	/**
	 * Returns a JndiDataSourceType enumerated value based on the GemFire preferred name for the supported JNDI,
	 * DataSource implementation, ignoring case and all extra leading/trailing whitespace.
	 *
	 * @param name the GemFire named JNDI DataSource implementation.
	 * @return the JndiDataSourceType enumerated value matching the given GemFire name used for the supported JNDI,
	 * DataSource implementation, or null if not match was found.
	 * @see #values()
	 * @see #isMatch(JndiDataSourceType, String)
	 */
	public static JndiDataSourceType valueOfIgnoreCase(final String name) {
		for (JndiDataSourceType jndiDataSourceType : values()) {
			if (isMatch(jndiDataSourceType, name)) {
				return jndiDataSourceType;
			}
		}

		return null;
	}

	/**
	 * Determines whether the specified JndiDataSourceType enum and the given, supported GemFire 'named',
	 * JNDI DataSource implementation are a match.
	 *
	 * @param jndiDataSourceType the given JndiDataSourceType enum used in the match.
	 * @param name the specified GemFire "named" JNDI DataSource implementation.
	 * @return a boolean value indicating whether the given JndiDataSourceType enumerated value matched the given name.
	 * @see java.lang.String#equalsIgnoreCase(String)
	 * @see org.springframework.util.StringUtils#trimWhitespace(String)
	 */
	private static boolean isMatch(final JndiDataSourceType jndiDataSourceType, String name) {
		name = StringUtils.trimWhitespace(name);
		return (jndiDataSourceType.getName().equalsIgnoreCase(name)
			|| jndiDataSourceType.name().equalsIgnoreCase(name));
	}

	/**
	 * Gets the GemFire name of the support JNDI DataSource implementation type.
	 *
	 * @return the GemFire named JNDI DataSource implementation.
	 */
	public String getName() {
		return name;
	}

	/**
	 * Returns a String describing this JNDI DataSource implementation based on the GemFire supported names.
	 *
	 * @return a String description for this JNDI DataSource (implementation) type.
	 */
	@Override
	public String toString() {
		return getName();
	}

}
