/*
 * Copyright 2010-2019 the original author or authors.
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Properties;
import javax.sql.DataSource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.data.gemfire.repository.sample.User;
import org.springframework.data.gemfire.test.support.DataSourceAdapter;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.Assert;

/**
 * The LazyWiringDeclarableSupportIntegrationTest class is a test suite of integration test cases testing
 * a LazyWiringDeclarableSupport object/component's wiring configuration and initialization in
 * a Spring container context.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.3.4
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class LazyWiringDeclarableSupportIntegrationTest {

	@Autowired
	private ApplicationContext applicationContext;

	protected static Properties createParameters(final String parameter, final String value) {
		Properties parameters = new Properties();
		parameters.setProperty(parameter, value);
		return parameters;
	}

	@Test
	public void testWiring() {
		TestDeclarable declarable = new TestDeclarable();

		declarable.init(createParameters("testParam", "testValue"));
		declarable.onApplicationEvent(new ContextRefreshedEvent(applicationContext));
		declarable.assertInitialized();

		assertNull(declarable.getDataSource());
		assertNotNull(declarable.getUser());
		assertEquals("supertool", declarable.getUser().getUsername());
	}

	@Test
	public void testWiringWithBeanTemplate() {
		TestDeclarable declarable = new TestDeclarable();

		declarable.init(createParameters(LazyWiringDeclarableSupport.BEAN_NAME_PARAMETER, "declarableTemplateBean"));
		declarable.onApplicationEvent(new ContextRefreshedEvent(applicationContext));
		declarable.assertInitialized();

		assertNotNull(declarable.getDataSource());
		assertNotNull(declarable.getUser());
		assertEquals("supertool", declarable.getUser().getUsername());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testWiringWithNonExistingBeanTemplate() {
		TestDeclarable declarable = new TestDeclarable();

		try {
			declarable.init(createParameters(LazyWiringDeclarableSupport.BEAN_NAME_PARAMETER,
				"nonExistingBeanTemplate"));
			declarable.onApplicationEvent(new ContextRefreshedEvent(applicationContext));
		}
		catch (IllegalArgumentException expected) {
			assertTrue(expected.getMessage().startsWith(
				"No bean with name 'nonExistingBeanTemplate' was found in the Spring context"));
			throw expected;
		}
	}

	protected static final class TestDataSource extends DataSourceAdapter {
	}

	protected static final class TestDeclarable extends LazyWiringDeclarableSupport {

		private DataSource dataSource;

		@Autowired
		private User user;

		public final void setDataSource(final DataSource dataSource) {
			this.dataSource = dataSource;
		}

		protected DataSource getDataSource() {
			return dataSource;
		}

		protected User getUser() {
			Assert.state(user != null, "A reference to the User was not properly configured!");
			return user;
		}
	}

}
