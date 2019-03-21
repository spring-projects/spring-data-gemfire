/*
 * Copyright 2010-2013 the original author or authors.
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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
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
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.LazyWiringDeclarableSupport
 * @since 1.3.4
 */
public class LazyWiringDeclarableSupportTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	protected static void assertParameters(Properties parameters, String expectedKey, String expectedValue) {
		assertThat(parameters, is(notNullValue()));
		assertThat(parameters.containsKey(expectedKey), is(true));
		assertThat(parameters.getProperty(expectedKey), is(equalTo(expectedValue)));
	}

	protected static Properties createParameters(final String parameter, final String value) {
		Properties parameters = new Properties();
		parameters.setProperty(parameter, value);
		return parameters;
	}

	@Test
	public void assertInitialized() {
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

	@Test
	public void assertInitializedWhenUninitialized() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport() {
			@Override protected boolean isInitialized() {
				return false;
			}
		};

		try {
			expectedException.expect(IllegalStateException.class);
			expectedException.expectCause(is(nullValue(Throwable.class)));
			expectedException.expectMessage(String.format(
				"This Declarable object (%1$s) has not been properly configured and initialized",
					declarable.getClass().getName()));

			declarable.assertInitialized();
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void assertUninitialized() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport();

		try {
			declarable.assertUninitialized();
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void assertUninitializedWhenInitialized() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport() {
			@Override protected boolean isInitialized() {
				return true;
			}
		};

		try {
			expectedException.expect(IllegalStateException.class);
			expectedException.expectCause(is(nullValue(Throwable.class)));
			expectedException.expectMessage(String.format(
				"This Declarable object (%1$s) has already been configured and initialized",
				declarable.getClass().getName()));

			declarable.assertUninitialized();
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void init() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport();

		try {
			assertThat(declarable.isInitialized(), is(false));

			declarable.init(createParameters("param", "value"));

			assertParameters(declarable.nullSafeGetParameters(), "param", "value");

			declarable.init(createParameters("newParam", "newValue"));

			assertParameters(declarable.nullSafeGetParameters(), "newParam", "newValue");
			assertThat(declarable.isInitialized(), is(false));
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void isInitialized() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport() {
			@Override protected boolean isInitialized() {
				return true;
			}
		};

		assertThat(declarable.isInitialized(), is(true));
		assertThat(declarable.isNotInitialized(), is(false));
	}

	@Test
	public void isUninitialized() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport() {
			@Override protected boolean isInitialized() {
				return false;
			}
		};

		assertThat(declarable.isInitialized(), is(false));
		assertThat(declarable.isNotInitialized(), is(true));
	}

	@Test
	public void locateBeanFactory() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class, "MockBeanFactory");

		GemfireBeanFactoryLocator locator = new GemfireBeanFactoryLocator();

		locator.setBeanName("MockBeanFactory");
		locator.setBeanFactory(mockBeanFactory);

		try {
			locator.afterPropertiesSet();

			assertThat(new TestLazyWiringDeclarableSupport().locateBeanFactory(null), is(sameInstance(mockBeanFactory)));
		}
		finally {
			locator.destroy();
		}
	}

	@Test
	public void nullSafeGetParametersWithNullReference() {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport();

		try {
			declarable.init(null);

			Properties parameters = declarable.nullSafeGetParameters();

			assertThat(parameters, is(notNullValue()));
			assertThat(parameters.isEmpty(), is(true));
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void onApplicationEvent() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"MockConfigurableApplicationContext");

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class,
			"MockConfigurableListableBeanFactory");

		when(mockApplicationContext.getBeanFactory()).thenReturn(mockBeanFactory);

		final AtomicBoolean doPostInitCalled = new AtomicBoolean(false);

		TestLazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport() {
			@Override protected void doPostInit(final Properties parameters) {
				super.doPostInit(parameters);
				assertInitialized();
				LazyWiringDeclarableSupportTest.assertParameters(parameters, "param", "value");
				doPostInitCalled.set(true);
			}
		};

		Properties parameters = createParameters("param", "value");

		try {
			declarable.init(parameters);
			declarable.onApplicationEvent(new ContextRefreshedEvent(mockApplicationContext));
			declarable.assertBeanFactory(mockBeanFactory);
			declarable.assertParameters(parameters);

			assertThat(declarable.isInitialized(), is(true));
			assertThat(doPostInitCalled.get(), is(true));

			verify(mockApplicationContext, times(1)).getBeanFactory();
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void onApplicationEventWithNullApplicationContext() throws Throwable {
		LazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport();

		try {
			ContextRefreshedEvent mockContextRefreshedEvent = mock(ContextRefreshedEvent.class, "MockContextRefreshedEvent");

			when(mockContextRefreshedEvent.getApplicationContext()).thenReturn(null);

			expectedException.expect(IllegalArgumentException.class);
			expectedException.expectCause(is(nullValue(Throwable.class)));
			expectedException.expectMessage("The Spring ApplicationContext (null) must be an instance of ConfigurableApplicationContext");

			declarable.onApplicationEvent(mockContextRefreshedEvent);
		}
		catch (Throwable t) {
			assertThat(declarable.isInitialized(), is(false));
			throw t;
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(declarable);
		}
	}

	@Test
	public void fullLifecycleOnApplicationEventToDestroy() throws Exception {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"MockConfigurableApplicationContext");

		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class,
			"MockConfigurableListableBeanFactory");

		when(mockApplicationContext.getBeanFactory()).thenReturn(mockBeanFactory);

		final AtomicBoolean doPostInitCalled = new AtomicBoolean(false);

		TestLazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport() {
			@Override protected void doPostInit(final Properties parameters) {
				super.doPostInit(parameters);
				assertInitialized();
				LazyWiringDeclarableSupportTest.assertParameters(parameters, "param", "value");
				doPostInitCalled.set(true);
			}
		};

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer();

		Properties parameters = createParameters("param", "value");

		try {
			declarable.init(parameters);

			assertThat(declarable.isInitialized(), is(false));
			assertThat(declarable.nullSafeGetParameters(), is(sameInstance(parameters)));
			assertThat(doPostInitCalled.get(), is(false));

			initializer.onApplicationEvent(new ContextRefreshedEvent(mockApplicationContext));

			assertThat(declarable.isInitialized(), is(true));
			assertThat(doPostInitCalled.get(), is(true));
			declarable.assertBeanFactory(mockBeanFactory);
			declarable.assertParameters(parameters);

			doPostInitCalled.set(false);
			declarable.destroy();

			assertThat(declarable.isInitialized(), is(false));
			assertThat(declarable.nullSafeGetParameters(), is(not(sameInstance(parameters))));
			assertThat(doPostInitCalled.get(), is(false));

			initializer.onApplicationEvent(new ContextRefreshedEvent(mockApplicationContext));

			assertThat(declarable.isInitialized(), is(false));
			assertThat(declarable.nullSafeGetParameters(), is(not(sameInstance(parameters))));
			assertThat(doPostInitCalled.get(), is(false));

			verify(mockApplicationContext, times(1)).getBeanFactory();
		}
		finally {
			initializer.onApplicationEvent(new ContextClosedEvent(mockApplicationContext));
		}
	}

	@Test
	public void initThenOnApplicationEventThenInitWhenInitialized() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class, "MockBeanFactory");

		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class, "MockApplicationContext");

		ConfigurableListableBeanFactory mockConfigurableListableBeanFactory = mock(ConfigurableListableBeanFactory.class,
			"MockConfigurableListableBeanFactory");

		when(mockApplicationContext.getBeanFactory()).thenReturn(mockConfigurableListableBeanFactory);

		GemfireBeanFactoryLocator locator = new GemfireBeanFactoryLocator();
		locator.setBeanName("MockBeanFactory");
		locator.setBeanFactory(mockBeanFactory);

		final AtomicBoolean doPostInitCalled = new AtomicBoolean(false);
		final AtomicReference<String> expectedKey = new AtomicReference<String>("testParam");
		final AtomicReference<String> expectedValue = new AtomicReference<String>("testValue");

		TestLazyWiringDeclarableSupport declarable = new TestLazyWiringDeclarableSupport() {
			@Override protected void doPostInit(final Properties parameters) {
				super.doPostInit(parameters);
				assertInitialized();
				LazyWiringDeclarableSupportTest.assertParameters(parameters, expectedKey.get(), expectedValue.get());
				doPostInitCalled.set(true);
			}
		};

		Properties parameters = createParameters("testParam", "testValue");

		try {
			locator.afterPropertiesSet();

			assertThat(declarable.isInitialized(), is(false));
			assertThat(declarable.nullSafeGetParameters(), is(not(sameInstance(parameters))));
			assertThat(doPostInitCalled.get(), is(false));

			declarable.init(parameters);
			declarable.assertBeanFactory(mockBeanFactory);
			declarable.assertParameters(parameters);

			assertThat(declarable.isInitialized(), is(true));
			assertThat(declarable.nullSafeGetParameters(), is(sameInstance(parameters)));
			assertThat(doPostInitCalled.get(), is(true));

			doPostInitCalled.set(false);
			declarable.onApplicationEvent(new ContextRefreshedEvent(mockApplicationContext));
			declarable.assertBeanFactory(mockBeanFactory);
			declarable.assertParameters(parameters);

			assertThat(declarable.isInitialized(), is(true));
			assertThat(declarable.nullSafeGetParameters(), is(sameInstance(parameters)));
			assertThat(doPostInitCalled.get(), is(true));

			doPostInitCalled.set(false);
			expectedKey.set("mockKey");
			expectedValue.set("mockValue");
			parameters = createParameters("mockKey", "mockValue");

			declarable.init(parameters);
			declarable.assertBeanFactory(mockBeanFactory);
			declarable.assertParameters(parameters);

			assertThat(declarable.isInitialized(), is(true));
			assertThat(declarable.nullSafeGetParameters(), is(sameInstance(parameters)));
			assertThat(doPostInitCalled.get(), is(true));

			verify(mockApplicationContext, times(1)).getBeanFactory();
		}
		finally {
			locator.destroy();
		}
	}

	protected static class TestLazyWiringDeclarableSupport extends LazyWiringDeclarableSupport {

		private BeanFactory actualBeanFactory;
		private Properties actualParameters;

		protected void assertBeanFactory(final BeanFactory expectedBeanFactory) {
			assertThat(actualBeanFactory, is(sameInstance(expectedBeanFactory)));
		}

		protected void assertParameters(final Properties expectedParameters) {
			assertThat(actualParameters, is(equalTo(expectedParameters)));
		}

		@Override
		void doInit(final BeanFactory beanFactory, final Properties parameters) {
			if (!isInitialized()) {
				this.actualBeanFactory = beanFactory;
				initialized = true;
			}

			this.actualParameters = parameters;
			doPostInit(parameters);
		}
	}

}
