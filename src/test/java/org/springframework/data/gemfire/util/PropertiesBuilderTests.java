/*
 * Copyright 2012-2018 the original author or authors.
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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Properties;

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link PropertiesBuilder} class.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.springframework.data.gemfire.util.PropertiesBuilder
 * @since 1.9.0
 */
public class PropertiesBuilderTests {

	protected Properties singletonProperties(String name, String value) {
		Properties properties = new Properties();
		properties.setProperty(name, value);
		return properties;
	}

	@Test
	@SuppressWarnings("unchecked")
	public void constructDefaultPropertiesBuilder() throws Exception {
		PropertiesBuilder builder = new PropertiesBuilder();

		Properties properties = builder.build();

		assertThat(properties, is(notNullValue(Properties.class)));
		assertThat(builder.getObject(), is(sameInstance(properties)));
		assertThat((Class<Properties>) builder.getObjectType(), is(equalTo(Properties.class)));
		assertThat(properties.isEmpty(), is(true));
	}

	@Test
	public void constructPropertiesBuilderWithDefaultProperties() {
		Properties defaults = singletonProperties("one", "1");

		PropertiesBuilder builder = new PropertiesBuilder(defaults);

		Properties properties = builder.build();

		assertThat(properties, is(notNullValue(Properties.class)));
		assertThat(properties, is(not(sameInstance(defaults))));
		assertThat(properties, is(equalTo(defaults)));
	}

	@Test
	public void constructPropertiesBuilderWithPropertiesBuilder() {
		PropertiesBuilder defaults = new PropertiesBuilder().setProperty("one", "1");
		PropertiesBuilder builder = new PropertiesBuilder(defaults);

		Properties properties = builder.build();

		assertThat(properties, is(notNullValue(Properties.class)));
		assertThat(properties.size(), is(equalTo(1)));
		assertThat(properties.containsKey("one"), is(true));
		assertThat(properties.getProperty("one"), is(equalTo("1")));
	}

	@Test
	public void fromInputStreamIsSuccessful() throws IOException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();

		Properties source = singletonProperties("one", "1");

		source.store(out, "fromInputStreamIsSuccessfulTest");

		Properties sink = PropertiesBuilder.from(new ByteArrayInputStream(out.toByteArray())).build();

		assertThat(sink, is(notNullValue(Properties.class)));
		assertThat(sink, is(not(sameInstance(source))));
		assertThat(sink, is(equalTo(source)));
	}

	@Test
	public void fromReaderIsSuccessful() throws IOException {
		StringWriter writer = new StringWriter();

		Properties source = singletonProperties("one", "1");

		source.store(writer, "fromReaderIsSuccessfulTest");

		Properties sink = PropertiesBuilder.from(new StringReader(writer.toString())).build();

		assertThat(sink, is(notNullValue(Properties.class)));
		assertThat(sink, is(not(sameInstance(source))));
		assertThat(sink, is(equalTo(source)));
	}

	@Test
	public void fromXmlInputStreamIsSuccessful() throws IOException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();

		Properties source = singletonProperties("one", "1");

		source.storeToXML(out, "fromXmlInputStreamIsSuccessfulTest");

		Properties sink = PropertiesBuilder.fromXml(new ByteArrayInputStream(out.toByteArray())).build();

		assertThat(sink, is(notNullValue(Properties.class)));
		assertThat(sink, is(not(sameInstance(source))));
		assertThat(sink, is(equalTo(source)));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void propertiesBuilderObjectTypeIsPropertiesClass() {
		assertThat((Class<Properties>) PropertiesBuilder.create().getObjectType(), is(equalTo(Properties.class)));
	}

	@Test
	public void propertiesBuilderIsSingletonIsTrue() {
		assertThat(new PropertiesBuilder().isSingleton(), is(true));
	}

	@Test
	public void addPropertiesFromPropertiesIsSuccessful() {
		PropertiesBuilder builder = PropertiesBuilder.create()
			.setProperty("one", "1")
			.setProperty("two", "@");

		Properties sink = builder.build();

		assertThat(sink, is(notNullValue(Properties.class)));
		assertThat(sink.size(), is(2));
		assertThat(sink.containsKey("one"), is(true));
		assertThat(sink.containsKey("two"), is(true));
		assertThat(sink.containsKey("three"), is(false));
		assertThat(sink.getProperty("one"), is(equalTo("1")));
		assertThat(sink.getProperty("two"), is(equalTo("@")));

		Properties source = new Properties();

		source.setProperty("two", "2");
		source.setProperty("three", "3");

		builder.add(source);

		sink = builder.build();

		assertThat(sink, is(notNullValue(Properties.class)));
		assertThat(sink.size(), is(equalTo(3)));
		assertThat(sink, is(not(sameInstance(source))));
		assertThat(sink.containsKey("one"), is(true));
		assertThat(sink.containsKey("two"), is(true));
		assertThat(sink.containsKey("three"), is(true));
		assertThat(sink.getProperty("one"), is(equalTo("1")));
		assertThat(sink.getProperty("two"), is(equalTo("2")));
		assertThat(sink.getProperty("three"), is(equalTo("3")));
	}

	@Test
	public void addPropertiesFromPropertiesBuilderIsSuccessful() {
		PropertiesBuilder source = PropertiesBuilder.create()
			.setProperty("one", "1")
			.setProperty("two", "2");

		Properties properties = PropertiesBuilder.create().add(source).build();

		assertThat(properties, is(notNullValue(Properties.class)));
		assertThat(properties.size(), is(equalTo(2)));
		assertThat(properties.containsKey("one"), is(true));
		assertThat(properties.containsKey("two"), is(true));
		assertThat(properties.getProperty("one"), is(equalTo("1")));
		assertThat(properties.getProperty("two"), is(equalTo("2")));
	}

	@Test
	public void setObjectPropertyValuesIsSuccessful() {
		Properties properties = PropertiesBuilder.create()
			.setProperty("boolean", Boolean.TRUE)
			.setProperty("character", 'A')
			.setProperty("integer", 1)
			.setProperty("double", Math.PI)
			.setProperty("string", (Object) "test")
			.build();

		assertThat(properties, is(notNullValue(Properties.class)));
		assertThat(properties.size(), is(equalTo(5)));
		assertThat(properties.getProperty("boolean"), is(equalTo(Boolean.TRUE.toString())));
		assertThat(properties.getProperty("character"), is(equalTo("A")));
		assertThat(properties.getProperty("integer"), is(equalTo("1")));
		assertThat(properties.getProperty("double"), is(equalTo(String.valueOf(Math.PI))));
		assertThat(properties.getProperty("string"), is(equalTo("test")));
	}

	@Test
	public void setObjectArrayPropertyValueIsSuccessful() {
		Properties properties = PropertiesBuilder.create()
			.setProperty("numbers", new Object[] { "one", "two", "three" })
			.build();

		assertThat(properties, is(notNullValue(Properties.class)));
		assertThat(properties.size(), is(equalTo(1)));
		assertThat(properties.containsKey("numbers"), is(true));
		assertThat(properties.getProperty("numbers"), is(equalTo("one,two,three")));
	}

	@Test
	public void setPropertyIgnoresNullObjectValue() {
		Properties properties = PropertiesBuilder.create().setProperty("object", (Object) null).build();

		assertThat(properties, is(notNullValue(Properties.class)));
		assertThat(properties.isEmpty(), is(true));
	}

	@Test
	public void setPropertyIgnoresEmptyAndNullLiteralStringValues() {
		Properties properties = PropertiesBuilder.create()
			.setProperty("blank", "  ")
			.setProperty("empty", "")
			.setProperty("null", "null")
			.setProperty("nullWithWhiteSpace", " null  ")
			.build();

		assertThat(properties, is(notNullValue(Properties.class)));
		assertThat(properties.isEmpty(), is(true));
	}

	@Test
	public void setPropertyIgnoresEmptyObjectArray() {
		Properties properties = PropertiesBuilder.create().setProperty("emptyArray", new Object[0]).build();

		assertThat(properties, is(notNullValue(Properties.class)));
		assertThat(properties.isEmpty(), is(true));
	}

	@Test
	public void setPropertyIgnoresNullObjectArray() {
		Properties properties = PropertiesBuilder.create().setProperty("nullArray", (Object[]) null).build();

		assertThat(properties, is(notNullValue(Properties.class)));
		assertThat(properties.isEmpty(), is(true));
	}

	@Test
	public void setStringPropertyValuesIsSuccessful() {
		Properties properties = PropertiesBuilder.create()
			.setProperty("one", "1")
			.setProperty("two", "2")
			.build();

		assertThat(properties, is(notNullValue(Properties.class)));
		assertThat(properties.size(), is(equalTo(2)));
		assertThat(properties.containsKey("one"), is(true));
		assertThat(properties.containsKey("two"), is(true));
		assertThat(properties.getProperty("one"), is(equalTo("1")));
		assertThat(properties.getProperty("two"), is(equalTo("2")));
	}

	@Test
	public void unsetPropertyIsSuccessful() {
		Properties properties = PropertiesBuilder.create().unsetProperty("example").build();

		assertThat(properties, is(notNullValue(Properties.class)));
		assertThat(properties.size(), is(equalTo(1)));
		assertThat(properties.containsKey("example"), is(true));
		assertThat(properties.getProperty("example"), is(equalTo("")));
	}

	@Test
	public void stringLiteralIsValuable() {
		assertThat(PropertiesBuilder.create().isValuable("test"), is(true));
	}

	@Test
	public void nullStringLiteralIsNotValuable() {
		assertThat(PropertiesBuilder.create().isValuable("null"), is(false));
		assertThat(PropertiesBuilder.create().isValuable("Null"), is(false));
		assertThat(PropertiesBuilder.create().isValuable("NULL"), is(false));
	}

	@Test
	public void emptyStringLiteralIsNotValuable() {
		assertThat(PropertiesBuilder.create().isValuable("  "), is(false));
		assertThat(PropertiesBuilder.create().isValuable(""), is(false));
	}
}
