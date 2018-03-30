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

import org.springframework.beans.factory.FactoryBean;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

/**
 * The PropertiesBuilder class is a Builder for {@link java.util.Properties}.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.springframework.beans.factory.FactoryBean
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public class PropertiesBuilder implements FactoryBean<Properties> {

	/**
	 * Factory method to create a default {@link PropertiesBuilder} instance.
	 *
	 * @return an instance of the {@link PropertiesBuilder} class with not {@link Properties}.
	 * @see #PropertiesBuilder()
	 */
	public static PropertiesBuilder create() {
		return new PropertiesBuilder();
	}

	/**
	 * Factory method to create an instance of {@link PropertiesBuilder} initialized with the given {@link Properties}.
	 *
	 * @param properties {@link Properties} used as the default properties of the constructed {@link PropertiesBuilder}.
	 * @return an instance of {@link PropertiesBuilder} initialized with the given {@link Properties}.
	 * @see java.util.Properties
	 * @see #PropertiesBuilder(Properties)
	 */
	public static PropertiesBuilder from(Properties properties) {
		return new PropertiesBuilder(properties);
	}

	/**
	 * Constructs and initializes a {@link PropertiesBuilder} containing all properties
	 * from the given {@link InputStream}.
	 *
	 * @param in {@link InputStream} source containing properties to use as the defaults for the constructed builder.
	 * @return a {@link PropertiesBuilder} initialized with properties from the given {@link InputStream}.
	 * @throws IllegalArgumentException if the {@link InputStream} cannot be read.
	 * @see java.io.InputStream
	 * @see java.util.Properties#load(InputStream)
	 */
	public static PropertiesBuilder from(InputStream in) {

		try {
			Properties defaults = new Properties();
			defaults.load(in);
			return new PropertiesBuilder(defaults);
		}
		catch (IOException cause) {
			throw new IllegalArgumentException("Failed to read properties from InputStream", cause);
		}
	}

	/**
	 * Constructs and initializes a {@link PropertiesBuilder} containing all properties
	 * from the given {@link Reader}.
	 *
	 * @param reader {@link Reader} source containing properties to use as the defaults for the constructed builder.
	 * @return a {@link PropertiesBuilder} initialized with properties from the given {@link Reader}.
	 * @throws IllegalArgumentException if the {@link Reader} cannot be read.
	 * @see java.io.Reader
	 * @see java.util.Properties#load(Reader)
	 */
	public static PropertiesBuilder from(Reader reader) {

		try {
			Properties defaults = new Properties();
			defaults.load(reader);
			return new PropertiesBuilder(defaults);
		}
		catch (IOException cause) {
			throw new IllegalArgumentException("Failed to read properties from Reader", cause);
		}
	}

	/**
	 * Constructs and initializes a {@link PropertiesBuilder} containing all properties
	 * from the given {@link InputStream} in XML format.
	 *
	 * @param xml {@link InputStream} source containing properties in XML format to use as defaults
	 * for the constructed builder.
	 * @return a {@link PropertiesBuilder} initialized with properties from the given XML {@link InputStream}.
	 * @throws IllegalArgumentException if the XML {@link InputStream} cannot be read.
	 * @see java.io.InputStream
	 * @see java.util.Properties#loadFromXML(InputStream)
	 */
	public static PropertiesBuilder fromXml(InputStream xml) {

		try {
			Properties defaults = new Properties();
			defaults.loadFromXML(xml);
			return new PropertiesBuilder(defaults);
		}
		catch (IOException cause) {
			throw new IllegalArgumentException("Failed to read properties from XML", cause);
		}
	}

	private final Properties properties;

	/**
	 * Constructs an instance of the {@link PropertiesBuilder} class.
	 */
	public PropertiesBuilder() {
		this.properties = new Properties();
	}

	/**
	 * Constructs an instance of the {@link PropertiesBuilder} class initialized with the default {@link Properties}.
	 *
	 * @param defaults {@link Properties} used as the defaults.
	 * @throws NullPointerException if the {@link Properties} reference is {@literal null}.
	 * @see java.util.Properties
	 */
	public PropertiesBuilder(Properties defaults) {
		this.properties = new Properties();
		this.properties.putAll(defaults);
	}

	/**
	 * Constructs an instance of the {@link PropertiesBuilder} class initialized with the given
	 * {@link PropertiesBuilder} providing the default {@link Properties} for this builder.
	 *
	 * @param builder {@link PropertiesBuilder} providing the default {@link Properties} for this builder.
	 * @throws NullPointerException if the {@link PropertiesBuilder} reference is {@literal null}.
	 * @see #PropertiesBuilder(Properties)
	 */
	public PropertiesBuilder(PropertiesBuilder builder) {
		this(builder.build());
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.FactoryBean#getObject()
	 */
	@Override
	public Properties getObject() throws Exception {
		return build();
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.FactoryBean#getObjectType()
	 */
	@Override
	public Class<?> getObjectType() {
		return this.properties != null ? this.properties.getClass() : Properties.class;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.FactoryBean#isSingleton()
	 */
	@Override
	public boolean isSingleton() {
		return true;
	}

	/**
	 * Null-safe method to add all the {@link Properties} to this builder.  This operation effectively overwrites
	 * any properties already set with the same name from the source.
	 *
	 * @param properties {@link Properties} to add to this builder.
	 * @return a reference to this {@link PropertiesBuilder}.
	 * @see java.util.Properties
	 */
	public PropertiesBuilder add(Properties properties) {

		if (!CollectionUtils.isEmpty(properties)) {
			this.properties.putAll(properties);
		}

		return this;
	}

	/**
	 * Null-safe method to add all the {@link Properties} from the provided {@link PropertiesBuilder} to this builder.
	 * This operation effectively overwrites any properties already set with the same name from the source.
	 *
	 * @param builder source of the {@link Properties} to add to this builder.
	 * @return a reference to this {@link PropertiesBuilder}.
	 * @see org.springframework.data.gemfire.util.PropertiesBuilder
	 */
	public PropertiesBuilder add(PropertiesBuilder builder) {
		return builder != null ? add(builder.build()) : this;
	}

	/**
	 * Sets a property with given name to the specified value.  The property is only set if the value is not null.
	 *
	 * @param name the name of the property to set.
	 * @param value the value to set the property to.
	 * @return a reference to this {@link PropertiesBuilder}.
	 * @see #setProperty(String, String)
	 */
	public PropertiesBuilder setProperty(String name, Object value) {
		return value != null ? setProperty(name, value.toString()) : this;
	}

	/**
	 * Sets the named property to the given array of object values.  The property is only set
	 * if the array of object value is not null or empty.
	 *
	 * @param name name of the property to set.
	 * @param values array of object values used as the property's value.
	 * @return a reference to this {@link PropertiesBuilder}
	 * @see org.springframework.util.StringUtils#arrayToCommaDelimitedString(Object[])
	 * @see #setProperty(String, String)
	 */
	public PropertiesBuilder setProperty(String name, Object[] values) {
		return !ObjectUtils.isEmpty(values) ? setProperty(name, StringUtils.arrayToCommaDelimitedString(values)) : this;
	}

	/**
	 * Sets a property with the given name to the specified {@link String} value.  The property is only set
	 * if the value is not {@literal null}, an empty {@link String} or not equal to the {@link String} literal
	 * {@literal null}, ignoring case.
	 *
	 * @param name the name of the property to set.
	 * @param value the value to set the property to.
	 * @return a reference to this {@link PropertiesBuilder}.
	 * @throws IllegalArgumentException if the property name is not specified.
	 * @see java.util.Properties#setProperty(String, String)
	 */
	public PropertiesBuilder setProperty(String name, String value) {

		Assert.hasText(name, String.format("Name [%s] must be specified", name));

		if (isValuable(value)) {
			this.properties.setProperty(name, value);
		}

		return this;
	}

	/**
	 * Sets the named property to the given {@literal value} if the {@literal defaultValue} is not null
	 * and {@literal value} is not equal to {@literal defaultValue}.
	 *
	 * @param <T> Class type of the property value.
	 * @param name name of the property to set.
	 * @param value value to set for the property.
	 * @param defaultValue default value of the property to compare with the given value
	 * when determining whether to set the property.
	 * @return a reference to this {@link PropertiesBuilder}.
	 * @see #setProperty(String, Object)
	 */
	public <T> PropertiesBuilder setPropertyIfNotDefault(String name, Object value, T defaultValue) {
		return defaultValue == null || !defaultValue.equals(value) ? setProperty(name, value) : this;
	}

	/**
	 * Un-sets the named property.  This method sets the given named property to an empty {@link String}.
	 *
	 * @param name name of the property to unset.
	 * @return a reference to this {@link PropertiesBuilder}.
	 * @throws IllegalArgumentException if the property name is not specified.
	 */
	public PropertiesBuilder unsetProperty(String name) {

		Assert.hasText(name, String.format("Name [%s] mut be specified", name));

		this.properties.setProperty(name, "");

		return this;
	}

	/**
	 * Determine whether the given {@link String} value is a valid {@link Properties} value.  A property value is
	 * considered valid if it is not null, not empty and not equal to (case-insensitive) {@link String} literal
	 * {@literal null}.
	 *
	 * @param value {@link String} value for the property being set.
	 * @return a boolean value indicating whether the given {@link String} value is a valid {@link Properties} value.
	 */
	protected boolean isValuable(String value) {
		return StringUtils.hasText(value) && !"null".equalsIgnoreCase(value.trim());
	}

	/**
	 * Builds the {@link Properties} object from this builder.
	 *
	 * @return the {@link Properties} object built by this builder.
	 * @see java.util.Properties
	 */
	public Properties build() {
		return this.properties;
	}
}
