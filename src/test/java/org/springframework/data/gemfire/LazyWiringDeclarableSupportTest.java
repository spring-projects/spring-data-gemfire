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
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
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
			assertEquals(String.format("This Declarable object (%1$s) has not ben properly configured and initialized!",
				TestLazyWiringDeclarableSupport.class.getName()), expected.getMessage());
			throw expected;
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void testInit() {
		new TestLazyWiringDeclarableSupport().init(createParameters("testParam1", "testValue1"));
	}

	@Test(expected = IllegalStateException.class)
	public void testInitWhenInitialized() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport();

		try {
			declarable.init(createParameters("testParam1", "testValue1"));
		}
		catch (IllegalStateException unexpected) {
			fail("Calling init the first time should not throw an IllegalStateException!");
		}

		try {
			declarable.init(createParameters("testParam2", "testValue1"));
		}
		catch (IllegalStateException expected) {
			assertEquals(String.format("This Declarable (%1$s) has already been initialized.",
				declarable.getClass().getName()), expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalStateException.class)
	public void testOnApplicationEventWithNoParameters() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport();

		try {
			declarable.onApplicationEvent(new ContextRefreshedEvent(mock(ConfigurableApplicationContext.class,
				"testOnApplicationEventWithNoParameters")));
		}
		catch (IllegalStateException expected) {
			assertEquals(String.format("This Declarable object's (%1$s) init method has not been invoked!",
				declarable.getClass().getName()), expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testOnApplicationEventWithNonConfigurableApplicationContext() {
		ApplicationContext mockApplicationContext = mock(ApplicationContext.class,
			"testOnApplicationEventWithNonConfigurableApplicationContext");

		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport();

		try {
			declarable.init(createParameters("testParam1", "testValue1"));
			declarable.onApplicationEvent(new ContextRefreshedEvent(mockApplicationContext));
		}
		catch (IllegalArgumentException expected) {
			assertEquals(String.format("The Spring ApplicationContext (%1$s) must be an instance of ConfigurableApplicationContext.",
				mockApplicationContext.getClass().getName()), expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testOnApplicationEventAndDoPostInit() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testOnApplicationEventAndDoPostInit.ApplicationContext");

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class,
			"testOnApplicationEventAndDoPostInit.BeanFactory");

		when(mockApplicationContext.getBeanFactory()).thenReturn(mockBeanFactory);

		Properties testParameters = createParameters("testParam1", "testValue1");

		final AtomicBoolean doPostInitCalled = new AtomicBoolean(false);

		TestLazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport() {
			@Override protected void doPostInit(final Properties parameters) {
				assertInitialized();
				doPostInitCalled.set(true);
			}
		};

		declarable.init(testParameters);
		declarable.onApplicationEvent(new ContextRefreshedEvent(mockApplicationContext));
		declarable.assertEquals(testParameters);
		declarable.assertSame(mockBeanFactory);

		assertTrue(doPostInitCalled.get());
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
