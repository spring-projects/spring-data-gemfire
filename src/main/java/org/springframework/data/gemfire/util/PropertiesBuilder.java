/*
 * Copyright 2012 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.Properties;

import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * The PropertiesBuilder class is a Builder for {@link java.util.Properties}.
 *
 * @author John Blum
 * @see java.util.Properties
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PropertiesBuilder {

	private final Properties properties;

	public PropertiesBuilder() {
		this.properties = new Properties();
	}

	public PropertiesBuilder(Properties defaults) {
		this.properties = new Properties(defaults);
	}

	public PropertiesBuilder(PropertiesBuilder builder) {
		this(builder != null ? builder.build() : null);
	}

	public PropertiesBuilder add(Properties properties) {
		if (!CollectionUtils.isEmpty(properties)) {
			this.properties.putAll(properties);
		}

		return this;
	}

	public PropertiesBuilder add(PropertiesBuilder builder) {
		if (builder != null) {
			add(builder.build());
		}

		return this;
	}

	public PropertiesBuilder from(InputStream in) {
		try {
			this.properties.load(in);
			return this;
		}
		catch (IOException e) {
			throw new IllegalArgumentException("Failed to read properties from InputStream", e);
		}
	}

	public PropertiesBuilder from(Reader reader) {
		try {
			this.properties.load(reader);
			return this;
		}
		catch (IOException e) {
			throw new IllegalArgumentException("Failed to read properties from Reader", e);
		}
	}

	public PropertiesBuilder fromXml(InputStream xml) {
		try {
			this.properties.loadFromXML(xml);
			return this;
		}
		catch (IOException e) {
			throw new IllegalArgumentException("Failed to read properties from XML InputStream", e);
		}
	}

	public PropertiesBuilder setProperty(String name, Object value) {
		if (value != null) {
			setProperty(name, value.toString());
		}

		return this;
	}

	public PropertiesBuilder setProperty(String name, String value) {
		Assert.hasText(name, String.format("Name [%s] must not be null or empty", name));

		if (StringUtils.hasText(value)) {
			this.properties.setProperty(name, value);
		}

		return this;
	}

	public Properties build() {
		return this.properties;
	}
}
