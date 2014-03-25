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

package org.springframework.data.gemfire.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Properties;

import org.junit.After;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationListener;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.util.ObjectUtils;

/**
 * The SpringContextBootstrappingInitializerTest class is a test suite of test cases testing the contract
 * and functionality of the SpringContextBootstrappingInitializer class.  This test class focuses on testing isolated
 * units of functionality in the Initializer class directly, mocking any dependencies as appropriate, in order for the
 * class to uphold it's contract.
 * <p/>
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer
 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializerIntegrationTest
 * @since 1.3.4
 */
@SuppressWarnings("unused")
public class SpringContextBootstrappingInitializerTest {

	protected static Properties createParameters(final String parameter, final String value) {
		Properties parameters = new Properties();
		parameters.setProperty(parameter, value);
		return parameters;
	}

	protected static Properties createParameters(final Properties parameters, final String parameter, final String value) {
		parameters.setProperty(parameter, value);
		return parameters;
	}

	@After
	public void tearDown() {
		SpringContextBootstrappingInitializer.applicationContext = null;
	}

	@Test(expected = IllegalArgumentException.class)
	public void testCreateApplicationContextWhenBasePackagesAndConfigLocationsAreBothUnspecified() {
		try {
			new SpringContextBootstrappingInitializer().createApplicationContext(null, null);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Either 'basePackages' or 'configLocations' must be specified to construct an instance of the ConfigurableApplicationContext.",
				expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testCreateClassPathXmlApplicationContextWhenConfigLocationsAreUnspecified() {
		try {
			new SpringContextBootstrappingInitializer().createApplicationContext(null);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("'configLocations' must be specified to construct an instance of the ClassPathXmlApplicationContext.",
				expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testInitWithUnspecifiedBasePackagesAndContextConfigLocationsParameter() {
		try {
			new SpringContextBootstrappingInitializer().init(createParameters(createParameters(
				SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER, ""),
				SpringContextBootstrappingInitializer.BASE_PACKAGES_PARAMETER, ""));
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Either 'basePackages' or the 'contextConfigLocations' parameter must be specified.",
				expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalStateException.class)
	public void testInitWithExistingApplicationContext() {
		try {
			ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
				"testInitWithExistingApplicationContext");

			when(mockApplicationContext.getId()).thenReturn("testInitWithExistingApplicationContext");

			SpringContextBootstrappingInitializer.applicationContext = mockApplicationContext;

			new SpringContextBootstrappingInitializer().init(createParameters(
				SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER,
					"/path/to/spring/context/configuration/file.xml"));
		}
		catch (IllegalStateException expected) {
			assertEquals("A Spring application context with ID (testInitWithExistingApplicationContext) has already been created.",
				expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testInitWhenCreateApplicationContextReturnsNull() {
		try {
			SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {
				@Override
				protected ConfigurableApplicationContext createApplicationContext(final String[] basePackages,
						final String[] configLocations) {
					return null;
				}
			};

			initializer.init(createParameters(SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER,
				"/path/to/spring/context/configuration/file.xml"));
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The 'created' ConfigurableApplicationContext cannot be null!", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalStateException.class)
	public void testInitWithNonActiveApplicationContext() {
		final ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testInitWithNonActiveApplicationConstruct");

		when(mockApplicationContext.getId()).thenReturn("testInitWithNonActiveApplicationContext");
		when(mockApplicationContext.isActive()).thenReturn(false);

		SpringContextBootstrappingInitializer initializer = null;

		try {
			initializer = new SpringContextBootstrappingInitializer() {
				@Override
				protected ConfigurableApplicationContext createApplicationContext(final String[] basePackages,
						final String[] configLocations) {
					return mockApplicationContext;
				}
			};

			initializer.init(createParameters(SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER,
				"/path/to/spring/context/configuration/file.xml"));
		}
		catch (IllegalStateException expected) {
			assertEquals("The Spring application context (testInitWithNonActiveApplicationContext) has failed to be properly initialized with the following config files ([/path/to/spring/context/configuration/file.xml]) or base packages ([])!",
				expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockApplicationContext, times(1)).addApplicationListener(eq(initializer));
			verify(mockApplicationContext, times(1)).registerShutdownHook();
			verify(mockApplicationContext, times(1)).refresh();
		}
	}

	@Test
	public void testInitFollowedByGetApplicationContext() {
		final ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testInitFollowedByGetApplicationContext");

		when(mockApplicationContext.getId()).thenReturn("testInitFollowedByGetApplicationContext");
		when(mockApplicationContext.isActive()).thenReturn(true);

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {
				@Override
				protected ConfigurableApplicationContext createApplicationContext(final String[] basePackages,
						final String[] configLocations) {
					return mockApplicationContext;
				}
			};

		initializer.init(createParameters(SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER,
			"/path/to/spring/context/configuration/file.xml"));

		verify(mockApplicationContext, times(1)).addApplicationListener(eq(initializer));
		verify(mockApplicationContext, times(1)).registerShutdownHook();
		verify(mockApplicationContext, times(1)).refresh();

		assertSame(mockApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());
	}

	@Test(expected = IllegalStateException.class)
	public void testGetApplicationContextUninitialized() {
		try {
			SpringContextBootstrappingInitializer.getApplicationContext();
		}
		catch (IllegalStateException expected) {
			assertEquals("The Spring ApplicationContext has not been created!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testNullSafeGetApplicationContextIdWithNullReference() {
		assertNull(new SpringContextBootstrappingInitializer().nullSafeGetApplicationContextId(null));
	}

	@Test
	public void testNullSafeGetApplicationContextIdWithNonNullReference() {
		ApplicationContext mockApplicationContext = mock(ApplicationContext.class,
			"testNullSafeGetApplicationContextIdWithNonNullReference");

		when(mockApplicationContext.getId()).thenReturn("testNullSafeGetApplicationContextIdWithNonNullReference");

		assertEquals("testNullSafeGetApplicationContextIdWithNonNullReference",
			new SpringContextBootstrappingInitializer().nullSafeGetApplicationContextId(mockApplicationContext));
	}

	@Test
	public void testRegisterAndOnApplicationEvent() {
		TestApplicationListener testListener = SpringContextBootstrappingInitializer.register(
			new TestApplicationListener());

		try {
			ContextRefreshedEvent testEvent = new ContextRefreshedEvent(mock(ApplicationContext.class,
				"testRegisterAndOnApplicationEvent"));

			new SpringContextBootstrappingInitializer().onApplicationEvent(testEvent);

			testListener.assertCalled();
			testListener.assertSame(testEvent);
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testListener);
		}
	}

	@Test
	public void testRegisterUnregisterAndOnApplicationEvent() {
		TestApplicationListener testListener = SpringContextBootstrappingInitializer.unregister(
			SpringContextBootstrappingInitializer.register(new TestApplicationListener()));

		try {
			ContextRefreshedEvent testEvent = new ContextRefreshedEvent(mock(ApplicationContext.class,
				"testRegisterUnregisterAndOnApplicationEvent"));

			new SpringContextBootstrappingInitializer().onApplicationEvent(testEvent);

			testListener.assertNotCalled();
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testListener);
		}
	}

	@Test
	public void testNotifyListenersOnContextRefreshedEventBeforeApplicationContextExists() {
		TestApplicationListener testApplicationListener = new TestApplicationListener();

		SpringContextBootstrappingInitializer.applicationContext = null;
		SpringContextBootstrappingInitializer.register(testApplicationListener);

		testApplicationListener.assertNotCalled();
	}

	@Test
	public void testNotifyListenersOnContextRefreshedEventAfterApplicationContextRefreshed() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testNotifyListenersOnContextRefreshedEventAfterApplicationContextRefreshed");

		ContextRefreshedEvent testEvent = new ContextRefreshedEvent(mockApplicationContext);

		SpringContextBootstrappingInitializer.applicationContext = mockApplicationContext;
		new SpringContextBootstrappingInitializer().onApplicationEvent(testEvent);

		TestApplicationListener testApplicationListener = new TestApplicationListener();

		SpringContextBootstrappingInitializer.register(testApplicationListener);

		testApplicationListener.assertCalled();
		testApplicationListener.assertSame(testEvent);
	}

	// TODO add additional multi-thread test cases once MultithreadedTC test framework is added to the SDP project
	// to properly test concurrency of the notification and registration during Spring ApplicationContext creation.

	protected static class TestApplicationListener implements ApplicationListener<ContextRefreshedEvent> {

		private volatile boolean called = false;

		private volatile ContextRefreshedEvent actualEvent;

		public void assertCalled() {
			assertTrue(String.format("Expected the (%1$s).onApplicationEvent(:ContextRefreshedEvent) method to be called!",
				getClass().getName()), called);
		}

		public void assertNotCalled() {
			assertFalse(String.format("Expected the (%1$s).onApplicationEvent(:ContextRefreshedEvent) method to not be called for ApplicationEvent (%2$s)!",
				getClass().getName(), ObjectUtils.nullSafeClassName(actualEvent)), called);
		}

		public void assertSame(final ContextRefreshedEvent expectedEvent) {
			Assert.assertSame(expectedEvent, actualEvent);
		}

		@Override
		public void onApplicationEvent(final ContextRefreshedEvent event) {
			this.actualEvent = event;
			called = true;
		}
	}

}
