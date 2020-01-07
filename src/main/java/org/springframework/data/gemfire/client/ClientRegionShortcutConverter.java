/*
 * Copyright 2010-2020 the original author or authors.
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

package org.springframework.data.gemfire.client;

import org.apache.geode.cache.client.ClientRegionShortcut;

import org.springframework.core.convert.converter.Converter;

/**
 * The ClientRegionShortcutConverter class is a Spring Converter implementation converting String value Client Region
 * Shortcut representations into actual Pivotal GemFire ClientRegionShortcut enumerated values.
 *
 * @author John Blum
 * @see org.springframework.core.convert.converter.Converter
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @since 1.3.4
 */
@SuppressWarnings("unused")
public class ClientRegionShortcutConverter implements Converter<String, ClientRegionShortcut> {

	/**
	 * Converts the String value to upper case, trimming all whitespace.  This method guards against null values
	 * and returns the "null" String if value is null.
	 *
	 * @param value the String to convert to a trimmed, upper case value.
	 * @return a trimmed, upper case value of the specified String, or "null" if the String value reference is null.
	 * @see java.lang.String#toUpperCase()
	 * @see java.lang.String#trim()
	 * @see java.lang.String#valueOf(Object)
	 */
	protected static String toUpperCase(final String value) {
		return (value != null ? value.toUpperCase().trim() : String.valueOf(value));
	}

	/**
	 * Converts the source String representation of a Client Region Shortcut into a ClientRegionShortcut enumerated
	 * value.
	 *
	 * @param source the String representation of the Client Region Shortcut to convert.
	 * @return a ClientRegionShortcut enumerated value for the String representation.
	 * @throws IllegalArgumentException if the String source is not a valid ClientRegionShortcut enumerated value.
	 * @see org.apache.geode.cache.client.ClientRegionShortcut#valueOf(String)
	 */
	@Override
	public ClientRegionShortcut convert(final String source) {
		return ClientRegionShortcut.valueOf(toUpperCase(source));
	}

}
