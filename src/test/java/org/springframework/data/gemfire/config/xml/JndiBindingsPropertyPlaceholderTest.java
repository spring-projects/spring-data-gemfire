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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.util.List;
import java.util.Map;

import org.apache.geode.internal.datasource.ConfigProperty;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The JndiBindingsPropertyPlaceholderTest class is a test suite of test cases testing the configuration of a GemFire
 * Cache JNDI DataSource using property placeholders.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.4.0
 * @since 7.0.1 (GemFire)
 */
@ContextConfiguration(locations = "jndi-binding-with-property-placeholders-ns.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class JndiBindingsPropertyPlaceholderTest {

	@Autowired
	@SuppressWarnings("unused")
	private ApplicationContext context;

	protected void assertPropertyValueExists(final String expectedPropertyName, final String expectedPropertyValue,
			final List<ConfigProperty> properties) {
		for (ConfigProperty property : properties) {
			if (expectedPropertyName.equals(property.getName())) {
				assertEquals(expectedPropertyValue, property.getValue());
				assertEquals(String.class.getName(), property.getType());
				return;
			}
		}

		fail(String.format("ConfigProperty with name [%1$s] was not found!", expectedPropertyName));
	}

	@Test
	public void testCacheJndiDataSourceConfiguration() {
		CacheFactoryBean factory = context.getBean("&gemfireCache", CacheFactoryBean.class);

		List<CacheFactoryBean.JndiDataSource> jndiDataSources = factory.getJndiDataSources();

		assertNotNull(jndiDataSources);
		assertEquals(1, jndiDataSources.size());

		CacheFactoryBean.JndiDataSource dataSource = jndiDataSources.get(0);

		assertNotNull(dataSource);

		Map<String, String> attributes = dataSource.getAttributes();

		assertNotNull(attributes);
		assertFalse(attributes.isEmpty());
		assertEquals("testDataSource", attributes.get("jndi-name"));
		assertEquals("XAPooledDataSource", attributes.get("type"));
		assertEquals("60", attributes.get("blocking-timeout-seconds"));
		assertEquals("org.apache.derby.jdbc.EmbeddedConnectionPoolDataSource", attributes.get("conn-pooled-datasource-class"));
		assertEquals("jdbc:derby:testDataStore;create=true", attributes.get("connection-url"));
		assertEquals("180", attributes.get("idle-timeout-seconds"));
		assertEquals("10", attributes.get("init-pool-size"));
		assertEquals("org.apache.derby.jdbc.EmbeddedDriver", attributes.get("jdbc-driver-class"));
		assertEquals("30", attributes.get("login-timeout-seconds"));
		assertEquals("org.apache.derby.jdbc.NonExistingManagedConnectionFactoryClass", attributes.get("managed-connection-factory-class"));
		assertEquals("50", attributes.get("max-pool-size"));
		assertEquals("test123", attributes.get("password"));
		assertEquals("XATransaction", attributes.get("transaction-type"));
		assertEquals("masterdba", attributes.get("user-name"));
		assertEquals("org.apache.derby.jdbc.EmbeddedXADataSource", attributes.get("xa-datasource-class"));

		List<ConfigProperty> props = dataSource.getProps();

		assertNotNull(props);
		assertFalse(props.isEmpty());
		assertPropertyValueExists("schemaName", "testSchema", props);
		assertPropertyValueExists("databaseName", "testDataStore", props);
		assertPropertyValueExists("description", "test", props);
		assertPropertyValueExists("email", "masterdba@xcompany.com", props);
		assertPropertyValueExists("phone", "501-555-1234", props);
	}

}
