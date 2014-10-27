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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer;

/**
 * The LazyWiringDeclarableSupportTest class is a test suite of test cases testing the contract and functionality
 * of the LazyWiringDeclarableSupport class.  This test class focuses on testing isolated units of functionality
 * in the Declarable class directly, mocking any dependencies as appropriate, in order for the class to uphold
 * it's contract.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.LazyWiringDeclarableSupport
 * @since 1.3.4
 */
public class LazyWiringDeclarableSupportTest {

	protected static Properties createParameters(final String parameter, final String value) {
		Properties parameters = new Properties();
		parameters.setProperty(parameter, value);
		return parameters;
	}

	@Test
	public void testAssertInitialized() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport() {
			@Override protected boolean isInitialized() {
				return true;
			}
		};

		try {
			declarable.assertInitialized();
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test(expected = IllegalStateException.class)
	public void testAssertInitializedWhenUninitialized() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport();

		try {
			declarable.assertInitialized();
		}
		catch (IllegalStateException expected) {
			assertEquals(String.format("This Declarable object (%1$s) has not been properly configured and initialized!",
				TestLazyWiringDeclarableSupport.class.getName()), expected.getMessage());
			throw expected;
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void testAssertUninitialized() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport();

		try {
			declarable.assertUninitialized();
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test(expected = IllegalStateException.class)
	public void testAssertUninitializedWhenInitialized() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport() {
			@Override protected boolean isInitialized() {
				return true;
			}
		};

		try {
			declarable.assertUninitialized();
		}
		catch (IllegalStateException expected) {
			assertEquals(String.format(
				"This Declarable object (%1$s) has already been configured and initialized, and is currently active!",
				declarable.getClass().getName()), expected.getMessage());
			throw expected;
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void testInit() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport();

		try {
			assertFalse(declarable.isInitialized());
			declarable.init(createParameters("param", "value"));
			declarable.init(createParameters("newParam", "newValue"));
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test(expected = IllegalStateException.class)
	public void testInitWhenInitialized() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport() {
			@Override protected boolean isInitialized() {
				return true;
			}
		};

		try {
			declarable.init(createParameters("param", "value"));
		}
		catch (IllegalStateException expected) {
			assertEquals(String.format(
				"This Declarable object (%1$s) has already been configured and initialized, and is currently active!",
				declarable.getClass().getName()), expected.getMessage());
			throw expected;
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void testNullSafeGetParameters() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport();

		try {
			declarable.init(createParameters("param", "value"));

			Properties parameters = declarable.nullSafeGetParameters();

			assertNotNull(parameters);
			assertFalse(parameters.isEmpty());
			assertEquals(1, parameters.size());
			assertEquals("value", parameters.getProperty("param"));

			declarable.init(createParameters("newParam", "newValue"));
			parameters = declarable.nullSafeGetParameters();

			assertNotNull(parameters);
			assertFalse(parameters.isEmpty());
			assertEquals(1, parameters.size());
			assertFalse(parameters.containsKey("param"));
			assertEquals("newValue", parameters.getProperty("newParam"));
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void testNullSafeGetParametersWithNullReference() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport();

		try {
			Properties parameters = declarable.nullSafeGetParameters();

			assertNotNull(parameters);
			assertTrue(parameters.isEmpty());
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testOnApplicationEventWithNonConfigurableApplicationContext() {
		ApplicationContext mockApplicationContext = mock(ApplicationContext.class,
			"testOnApplicationEventWithNonConfigurableApplicationContext");

		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport();

		try {
			declarable.onApplicationEvent(new ContextRefreshedEvent(mockApplicationContext));
		}
		catch (IllegalArgumentException expected) {
			assertEquals(String.format("The Spring ApplicationContext (%1$s) must be an instance of ConfigurableApplicationContext.",
				mockApplicationContext.getClass().getName()), expected.getMessage());
			throw expected;
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void testOnApplicationEventAndDoPostInit() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testOnApplicationEventAndDoPostInit.ApplicationContext");

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class,
			"testOnApplicationEventAndDoPostInit.BeanFactory");

		when(mockApplicationContext.getBeanFactory()).thenReturn(mockBeanFactory);

		final AtomicBoolean doPostInitCalled = new AtomicBoolean(false);

		TestLazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport() {
			@Override protected void doPostInit(final Properties parameters) {
				super.doPostInit(parameters);
				assertInitialized();
				doPostInitCalled.set(true);
			}
		};

		Properties parameters = createParameters("param", "value");

		try {
			declarable.init(parameters);
			declarable.onApplicationEvent(new ContextRefreshedEvent(mockApplicationContext));
			declarable.assertEquals(parameters);
			declarable.assertSame(mockBeanFactory);

			assertTrue(doPostInitCalled.get());

			verify(mockApplicationContext, times(1)).getBeanFactory();
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void testFullLifecycleIntegrationWithDestroy() throws Exception {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testFullLifecycleIntegrationWithDestroy.ApplicationContext");

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class,
			"testFullLifecycleIntegrationWithDestroy.BeanFactory");

		when(mockApplicationContext.getBeanFactory()).thenReturn(mockBeanFactory);

		final AtomicBoolean doPostInitCalled = new AtomicBoolean(false);

		TestLazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport() {
			@Override protected void doPostInit(final Properties parameters) {
				super.doPostInit(parameters);
				assertInitialized();
				doPostInitCalled.set(true);
			}
		};

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer();

		Properties parameters = createParameters("param", "value");

		try {
			declarable.init(parameters);

			assertFalse(declarable.isInitialized());
			assertFalse(doPostInitCalled.get());
			assertSame(parameters, declarable.nullSafeGetParameters());

			initializer.onApplicationEvent(new ContextRefreshedEvent(mockApplicationContext));

			assertTrue(declarable.isInitialized());
			assertTrue(doPostInitCalled.get());
			declarable.assertEquals(parameters);
			declarable.assertSame(mockBeanFactory);

			declarable.destroy();
			doPostInitCalled.set(false);

			assertFalse(declarable.isInitialized());
			assertFalse(doPostInitCalled.get());
			assertNotSame(parameters, declarable.nullSafeGetParameters());

			initializer.onApplicationEvent(new ContextRefreshedEvent(mockApplicationContext));

			assertFalse(declarable.isInitialized());
			assertFalse(doPostInitCalled.get());
		}
		finally {
			initializer.onApplicationEvent(new ContextClosedEvent(mockApplicationContext));
		}
	}

	protected static class TestLazyWiringDeclarableSupport extends LazyWiringDeclarableSupport {

		private ConfigurableListableBeanFactory actualBeanFactory;
		private Properties actualParameters;

		protected void assertEquals(final Properties expectedParameters) {
			Assert.assertEquals(expectedParameters, actualParameters);
		}

		protected void assertSame(final ConfigurableListableBeanFactory expectedBeanFactory) {
			Assert.assertSame(expectedBeanFactory, actualBeanFactory);
		}

		@Override
		void doInit(final ConfigurableListableBeanFactory beanFactory, final Properties parameters) {
			this.actualBeanFactory = beanFactory;
			this.actualParameters = parameters;
			initialized = true;
			doPostInit(parameters);
		}
	}

}
