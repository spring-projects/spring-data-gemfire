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
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.junit.After;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.Matchers;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextException;
import org.springframework.context.ApplicationListener;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.ContextStartedEvent;
import org.springframework.context.event.ContextStoppedEvent;

/**
 * The SpringContextBootstrappingInitializerTest class is a test suite of test cases testing the contract
 * and functionality of the SpringContextBootstrappingInitializer class.  This test class focuses on testing isolated
 * units of functionality in the Initializer class directly, mocking any dependencies as appropriate, in order for the
 * class to uphold it's contract.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer
 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializerIntegrationTest
 * @since 1.4.0
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
		SpringContextBootstrappingInitializer.contextRefreshedEvent = null;
		SpringContextBootstrappingInitializer.unregister(TestAppConfigOne.class);
		SpringContextBootstrappingInitializer.unregister(TestAppConfigTwo.class);
	}

	@Test
	public void testGetApplicationContext() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testGetApplicationContext");

		SpringContextBootstrappingInitializer.applicationContext = mockApplicationContext;

		assertSame(mockApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());
	}

	@Test(expected = IllegalStateException.class)
	public void testGetApplicationContextUninitialized() {
		try {
			SpringContextBootstrappingInitializer.getApplicationContext();
		}
		catch (IllegalStateException expected) {
			assertEquals("The Spring ApplicationContext was not configured and initialized properly!",
				expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testCreateApplicationContextWhenBasePackagesAndConfigLocationsAreUnspecified() {
		try {
			new SpringContextBootstrappingInitializer().createApplicationContext(null, null);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("'basePackages', 'configLocations' or 'AnnotatedClasses' must be specified in order to"
				+ " construct and configure an instance of the ConfigurableApplicationContext", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testInitApplicationContext() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testInitApplicationContext");

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer();

		initializer.initApplicationContext(mockApplicationContext);

		verify(mockApplicationContext, times(1)).addApplicationListener(same(initializer));
		verify(mockApplicationContext, times(1)).registerShutdownHook();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testInitApplicationContextWithNullContext() {
		try {
			new SpringContextBootstrappingInitializer().initApplicationContext(null);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The ConfigurableApplicationContext reference must not be null!", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testRefreshApplicationContext() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testRefreshApplicationContext");

		new SpringContextBootstrappingInitializer().refreshApplicationContext(mockApplicationContext);

		verify(mockApplicationContext, times(1)).refresh();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRefreshApplicationContextWithNullContext() {
		try {
			new SpringContextBootstrappingInitializer().refreshApplicationContext(null);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The ConfigurableApplicationContext reference must not be null!", expected.getMessage());
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
	public void testRegisterAnnotatedClasses() {
		AnnotationConfigApplicationContext mockApplicationContext = mock(AnnotationConfigApplicationContext.class,
			"testRegisterAnnotatedClasses");

		Class<?>[] annotatedClasses = { TestAppConfigOne.class, TestAppConfigTwo.class };

		new SpringContextBootstrappingInitializer().registerAnnotatedClasses(mockApplicationContext, annotatedClasses);

		verify(mockApplicationContext, times(1)).register(annotatedClasses);
	}

	@Test
	public void testRegisterAnnotatedClassesWithEmptyAnnotatedClassesArray() {
		AnnotationConfigApplicationContext mockApplicationContext = mock(AnnotationConfigApplicationContext.class,
			"testRegisterAnnotatedClassesWithEmptyAnnotatedClassesArray");

		new SpringContextBootstrappingInitializer().registerAnnotatedClasses(mockApplicationContext, new Class<?>[0]);

		verify(mockApplicationContext, never()).register(any(Class.class));
	}

	@Test
	public void testScanBasePackages() {
		AnnotationConfigApplicationContext mockApplicationContext = mock(AnnotationConfigApplicationContext.class,
			"testScanBasePackages");

		String[] basePackages = { "org.example.app", "org.example.plugins" };

		new SpringContextBootstrappingInitializer().scanBasePackages(mockApplicationContext, basePackages);

		verify(mockApplicationContext, times(1)).scan(basePackages);
	}

	@Test
	public void testScanBasePackagesWithEmptyBasePackagesArray() {
		AnnotationConfigApplicationContext mockApplicationContext = mock(AnnotationConfigApplicationContext.class,
			"testScanBasePackages");

		new SpringContextBootstrappingInitializer().scanBasePackages(mockApplicationContext, null);

		verify(mockApplicationContext, never()).scan(any(String[].class));
	}

	private Class<?>[] annotatedClasses(final Class<?>... annotatedClasses) {
		return argThat(new ArgumentMatcher<Class<?>[]>() {
			@Override public boolean matches(final Object argument) {
				assertTrue(argument instanceof Class<?>[]);
				return Arrays.equals(annotatedClasses, (Class<?>[]) argument);
			}
		});
	}

	@Test
	public void testInitWithAnnotatedClasses() {
		final AnnotationConfigApplicationContext mockApplicationContext = mock(AnnotationConfigApplicationContext.class,
			"testInitWithAnnotatedClasses");

		doNothing().when(mockApplicationContext).addApplicationListener(any(ApplicationListener.class));
		doNothing().when(mockApplicationContext).registerShutdownHook();
		doNothing().when(mockApplicationContext).refresh();
		doNothing().when(mockApplicationContext).register(Matchers.<Class<?>[]>anyVararg());
		//doNothing().when(mockApplicationContext).register(annotatedClasses(TestAppConfigOne.class, TestAppConfigTwo.class));

		when(mockApplicationContext.getId()).thenReturn("testInitWithAnnotatedClasses");
		when(mockApplicationContext.isRunning()).thenReturn(true);

		assertNull(SpringContextBootstrappingInitializer.applicationContext);

		SpringContextBootstrappingInitializer.register(TestAppConfigOne.class);
		SpringContextBootstrappingInitializer.register(TestAppConfigTwo.class);

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {
			@Override protected ConfigurableApplicationContext createApplicationContext(String[] configLocations) {
				return mockApplicationContext;
			}
		};

		initializer.init(createParameters("test", "test"));

		verify(mockApplicationContext, times(1)).addApplicationListener(same(initializer));
		verify(mockApplicationContext, times(1)).registerShutdownHook();
		verify(mockApplicationContext, times(1)).register(TestAppConfigOne.class, TestAppConfigTwo.class);
		//verify(mockApplicationContext, times(1)).register(annotatedClasses(TestAppConfigOne.class, TestAppConfigTwo.class));
		//verify(mockApplicationContext, times(1)).register(Matchers.<Class<?>[]>anyVararg());
		verify(mockApplicationContext, never()).scan(any(String[].class));

		assertEquals(mockApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());
	}

	@Test
	public void testInitWithExistingApplicationContext() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testInitWithExistingApplicationContext");

		when(mockApplicationContext.isActive()).thenReturn(true);
		when(mockApplicationContext.getId()).thenReturn("testInitWithExistingApplicationContext");

		SpringContextBootstrappingInitializer.applicationContext = mockApplicationContext;

		assertSame(mockApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer();

		initializer.init(createParameters("test", "test"));

		verify(mockApplicationContext, never()).addApplicationListener(any(SpringContextBootstrappingInitializer.class));
		verify(mockApplicationContext, never()).registerShutdownHook();
		verify(mockApplicationContext, never()).refresh();

		assertSame(mockApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());
	}

	@Test
	public void testInitWhenApplicationContextIsNull() {
		assertNull(SpringContextBootstrappingInitializer.applicationContext);

		final ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testInitWhenApplicationContextIsNull");

		when(mockApplicationContext.getId()).thenReturn("testInitWhenApplicationContextIsNull");
		when(mockApplicationContext.isRunning()).thenReturn(true);

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {
			@Override
			protected ConfigurableApplicationContext createApplicationContext(final String[] basePackages,
					final String[] configLocations) {
				return mockApplicationContext;
			}
		};

		initializer.init(createParameters(SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER,
			"/path/to/spring/application/context.xml"));

		verify(mockApplicationContext, times(1)).addApplicationListener(same(initializer));
		verify(mockApplicationContext, times(1)).registerShutdownHook();
		verify(mockApplicationContext, times(1)).refresh();

		assertSame(mockApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());
	}

	@Test
	public void testInitWhenApplicationContextIsInactive() {
		ConfigurableApplicationContext mockInactiveApplicationContext = mock(ConfigurableApplicationContext.class,
			"testInitWhenApplicationContextIsInactive.Inactive");

		when(mockInactiveApplicationContext.isActive()).thenReturn(false);

		SpringContextBootstrappingInitializer.applicationContext = mockInactiveApplicationContext;

		assertSame(mockInactiveApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());

		final ConfigurableApplicationContext mockNewApplicationContext = mock(ConfigurableApplicationContext.class,
			"testInitWhenApplicationContextIsInactive.New");

		when(mockNewApplicationContext.getId()).thenReturn("testInitWhenApplicationContextIsInactive.New");
		when(mockNewApplicationContext.isRunning()).thenReturn(true);

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {
			@Override
			protected ConfigurableApplicationContext createApplicationContext(final String[] basePackages,
					final String[] configLocations) {
				return mockNewApplicationContext;
			}
		};

		initializer.init(createParameters(SpringContextBootstrappingInitializer.BASE_PACKAGES_PARAMETER,
			"org.example.app"));

		verify(mockNewApplicationContext, times(1)).addApplicationListener(same(initializer));
		verify(mockNewApplicationContext, times(1)).registerShutdownHook();
		verify(mockNewApplicationContext, times(1)).refresh();

		assertSame(mockNewApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testInitWhenBasePackagesAndContextConfigLocationsParametersAreUnspecified() throws Throwable {
		assertNull(SpringContextBootstrappingInitializer.applicationContext);

		try {
			new SpringContextBootstrappingInitializer().init(createParameters(createParameters(
				SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER, ""),
					SpringContextBootstrappingInitializer.BASE_PACKAGES_PARAMETER, "  "));
		}
		catch (ApplicationContextException expected) {
			assertTrue(expected.getMessage().contains("Failed to bootstrap the Spring ApplicationContext!"));
			assertTrue(expected.getCause() instanceof IllegalArgumentException);
			assertEquals("'basePackages', 'configLocations' or 'AnnotatedClasses' must be specified"
				+ " in order to construct and configure an instance of the ConfigurableApplicationContext",
					expected.getCause().getMessage());
			throw expected.getCause();
		}
	}

	@Test(expected = IllegalStateException.class)
	public void testInitWhenApplicationContextIsNotRunning() {
		assertNull(SpringContextBootstrappingInitializer.applicationContext);

		final ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testInitWhenApplicationContextIsNotRunning");

		when(mockApplicationContext.getId()).thenReturn("testInitWhenApplicationContextIsNotRunning");
		when(mockApplicationContext.isRunning()).thenReturn(false);

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {
			@Override
			protected ConfigurableApplicationContext createApplicationContext(final String[] basePackages,
					final String[] configLocations) {
				return mockApplicationContext;
			}
		};

		try {
			initializer.init(createParameters(SpringContextBootstrappingInitializer.BASE_PACKAGES_PARAMETER,
				"org.example.app, org.example.plugins"));

			SpringContextBootstrappingInitializer.getApplicationContext();
		}
		catch (ApplicationContextException expected) {
			assertTrue(expected.getMessage().contains("Failed to bootstrap the Spring ApplicationContext!"));
			assertTrue(expected.getCause() instanceof IllegalStateException);
			assertEquals("The Spring ApplicationContext (testInitWhenApplicationContextIsNotRunning) failed to be properly initialized with the context config files ([]) or base packages ([org.example.app, org.example.plugins])!",
				expected.getCause().getMessage());
			throw (IllegalStateException) expected.getCause();
		}
		finally {
			verify(mockApplicationContext, times(1)).addApplicationListener(same(initializer));
			verify(mockApplicationContext, times(1)).registerShutdownHook();
			verify(mockApplicationContext, times(1)).refresh();

			assertNull(SpringContextBootstrappingInitializer.applicationContext);
		}
	}

	@Test(expected = IllegalStateException.class)
	public void testInitLogsErrors() throws Throwable {
		final Log mockLog = mock(Log.class, "testInitLogsErrors.MockLog");

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {
			@Override protected Log initLogger() {
				return mockLog;
			}

			@Override protected ConfigurableApplicationContext createApplicationContext(final String[] basePackages,
					final String[] configLocations) {
				throw new IllegalStateException("TEST");
			}
		};

		try {
			initializer.init(createParameters(SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER,
				"classpath/to/spring/application/context.xml"));
		}
		catch (ApplicationContextException expected) {
			assertTrue(expected.getMessage().contains("Failed to bootstrap the Spring ApplicationContext!"));
			assertTrue(expected.getCause() instanceof IllegalStateException);
			assertEquals("TEST", expected.getCause().getMessage());
			throw expected.getCause();
		}
		finally {
			verify(mockLog, times(1)).error(eq("Failed to bootstrap the Spring ApplicationContext!"),
				any(RuntimeException.class));
		}
	}

	protected static void assertNotified(TestApplicationListener listener, ContextRefreshedEvent expectedEvent) {
		assertTrue(listener.isNotified());
		Assert.assertSame(expectedEvent, listener.getActualEvent());
	}

	protected static void assertUnnotified(TestApplicationListener listener) {
		assertFalse(listener.isNotified());
		assertNull(listener.getActualEvent());
	}

	@Test
	public void testOnApplicationEvent() {
		TestApplicationListener testApplicationListener = new TestApplicationListener("testOnApplicationEvent");

		try {
			testApplicationListener = SpringContextBootstrappingInitializer.register(testApplicationListener);

			assertUnnotified(testApplicationListener);

			ContextRefreshedEvent testContextRefreshedEvent = new ContextRefreshedEvent(mock(ApplicationContext.class,
				"testOnApplicationEvent"));

			new SpringContextBootstrappingInitializer().onApplicationEvent(testContextRefreshedEvent);

			assertNotified(testApplicationListener, testContextRefreshedEvent);
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testApplicationListener);
		}
	}

	@Test
	public void testOnApplicationEventWithContextStartedEvent() {
		TestApplicationListener testApplicationListener = new TestApplicationListener(
			"testOnApplicationEventWithContextStartedEvent");

		try {
			testApplicationListener = SpringContextBootstrappingInitializer.register(testApplicationListener);

			assertUnnotified(testApplicationListener);

			ContextStartedEvent testContextStartedEvent = mock(ContextStartedEvent.class,
				"testOnApplicationEventWithContextStartedEvent");

			new SpringContextBootstrappingInitializer().onApplicationEvent(testContextStartedEvent);

			assertUnnotified(testApplicationListener);
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testApplicationListener);
		}
	}

	@Test
	public void testOnApplicationEventWithMultipleRegisteredApplicationListeners() {
		TestApplicationListener testApplicationListenerOne = new TestApplicationListener(
			"testOnApplicationEventWithMultipleRegisteredApplicationListeners.1");

		TestApplicationListener testApplicationListenerTwo = new TestApplicationListener(
			"testOnApplicationEventWithMultipleRegisteredApplicationListeners.2");

		TestApplicationListener testApplicationListenerThree = new TestApplicationListener(
			"testOnApplicationEventWithMultipleRegisteredApplicationListeners.3");

		try {
			testApplicationListenerOne = SpringContextBootstrappingInitializer.register(testApplicationListenerOne);
			testApplicationListenerTwo = SpringContextBootstrappingInitializer.register(testApplicationListenerTwo);
			testApplicationListenerThree = SpringContextBootstrappingInitializer.register(testApplicationListenerThree);

			assertUnnotified(testApplicationListenerOne);
			assertUnnotified(testApplicationListenerTwo);
			assertUnnotified(testApplicationListenerThree);

			ContextRefreshedEvent testContextRefreshedEvent = new ContextRefreshedEvent(mock(ApplicationContext.class,
				"testOnApplicationEventWithMultipleRegisteredApplicationListeners"));

			new SpringContextBootstrappingInitializer().onApplicationEvent(testContextRefreshedEvent);

			assertNotified(testApplicationListenerOne, testContextRefreshedEvent);
			assertNotified(testApplicationListenerTwo, testContextRefreshedEvent);
			assertNotified(testApplicationListenerThree, testContextRefreshedEvent);
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testApplicationListenerOne);
			SpringContextBootstrappingInitializer.unregister(testApplicationListenerTwo);
			SpringContextBootstrappingInitializer.unregister(testApplicationListenerThree);
		}
	}

	@Test
	public void testOnApplicationEventWithUnregisteredApplicationListener() {
		TestApplicationListener testApplicationListener = new TestApplicationListener(
				"testOnApplicationEventWithUnregisteredApplicationListener");

		try {
			testApplicationListener = SpringContextBootstrappingInitializer.unregister(
				SpringContextBootstrappingInitializer.register(testApplicationListener));

			assertUnnotified(testApplicationListener);

			ContextRefreshedEvent testContextRefreshedEvent = new ContextRefreshedEvent(mock(ApplicationContext.class,
				"testRegisterThenUnregisterWithOnApplicationEvent"));

			new SpringContextBootstrappingInitializer().onApplicationEvent(testContextRefreshedEvent);

			assertUnnotified(testApplicationListener);
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testApplicationListener);
		}
	}

	@Test
	public void testNotifyOnExistingContextRefreshedEventBeforeApplicationContextExists() {
		assertNull(SpringContextBootstrappingInitializer.contextRefreshedEvent);

		TestApplicationListener testApplicationListener = new TestApplicationListener(
			"testNotifyOnExistingContextRefreshedEventBeforeApplicationContextExists");

		try {
			testApplicationListener = SpringContextBootstrappingInitializer.register(testApplicationListener);
			assertUnnotified(testApplicationListener);
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testApplicationListener);
		}
	}

	@Test
	public void testNotifyOnExistingContextRefreshedEventAfterContextRefreshed() {
		ContextRefreshedEvent testContextRefreshedEvent = new ContextRefreshedEvent(mock(ApplicationContext.class));

		new SpringContextBootstrappingInitializer().onApplicationEvent(testContextRefreshedEvent);

		TestApplicationListener testApplicationListener = new TestApplicationListener(
			"testNotifyOnExistingContextRefreshedEventAfterContextRefreshed");

		try {
			testApplicationListener = SpringContextBootstrappingInitializer.register(testApplicationListener);
			assertNotified(testApplicationListener, testContextRefreshedEvent);
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testApplicationListener);
   		}
	}

	@Test
	public void testOnApplicationEventAndNotifyOnExistingContextRefreshedEvent() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testOnApplicationEventAndNotifyOnExistingContextRefreshedEvent");

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer();

		TestApplicationListener testApplicationListenerOne = new TestApplicationListener(
			"testOnApplicationEventAndNotifyOnExistingContextRefreshedEvent.1");

		TestApplicationListener testApplicationListenerTwo = new TestApplicationListener(
			"testOnApplicationEventAndNotifyOnExistingContextRefreshedEvent.2");

		TestApplicationListener testApplicationListenerThree = new TestApplicationListener(
			"testOnApplicationEventAndNotifyOnExistingContextRefreshedEvent.3");

		try {
			testApplicationListenerOne = SpringContextBootstrappingInitializer.register(testApplicationListenerOne);

			assertUnnotified(testApplicationListenerOne);
			assertUnnotified(testApplicationListenerTwo);
			assertUnnotified(testApplicationListenerThree);

			ContextRefreshedEvent testContextRefreshedEvent = new ContextRefreshedEvent(mockApplicationContext);

			initializer.onApplicationEvent(testContextRefreshedEvent);

			assertNotified(testApplicationListenerOne, testContextRefreshedEvent);
			assertUnnotified(testApplicationListenerTwo);
			assertUnnotified(testApplicationListenerThree);

			testApplicationListenerTwo = SpringContextBootstrappingInitializer.register(testApplicationListenerTwo);

			assertNotified(testApplicationListenerTwo, testContextRefreshedEvent);
			assertUnnotified(testApplicationListenerOne);
			assertUnnotified(testApplicationListenerThree);

			ContextStoppedEvent testContextStoppedEvent = new ContextStoppedEvent(mockApplicationContext);

			initializer.onApplicationEvent(testContextStoppedEvent);

			assertUnnotified(testApplicationListenerOne);
			assertUnnotified(testApplicationListenerTwo);
			assertUnnotified(testApplicationListenerThree);

			initializer.onApplicationEvent(testContextRefreshedEvent);

			assertNotified(testApplicationListenerOne, testContextRefreshedEvent);
			assertNotified(testApplicationListenerTwo, testContextRefreshedEvent);
			assertUnnotified(testApplicationListenerThree);

			ContextClosedEvent testContextClosedEvent = new ContextClosedEvent(mockApplicationContext);

			initializer.onApplicationEvent(testContextClosedEvent);

			assertUnnotified(testApplicationListenerOne);
			assertUnnotified(testApplicationListenerTwo);
			assertUnnotified(testApplicationListenerThree);

			SpringContextBootstrappingInitializer.register(testApplicationListenerThree);

			assertUnnotified(testApplicationListenerOne);
			assertUnnotified(testApplicationListenerTwo);
			assertUnnotified(testApplicationListenerThree);
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testApplicationListenerOne);
			SpringContextBootstrappingInitializer.unregister(testApplicationListenerTwo);
			SpringContextBootstrappingInitializer.unregister(testApplicationListenerThree);
		}
	}

	@Configuration
	protected static class TestAppConfigOne {
	}

	@Configuration
	protected static class TestAppConfigTwo {
	}

	// TODO add additional multi-threaded test cases once MultithreadedTC test framework is added to the SDP project
	// in order to properly test concurrency of notification and registration during Spring ApplicationContext creation.

	protected static class TestApplicationListener implements ApplicationListener<ContextRefreshedEvent> {

		private volatile boolean notified = false;

		private volatile ContextRefreshedEvent actualEvent;

		private final String name;

		public TestApplicationListener(final String name) {
			this.name = name;
		}

		public ContextRefreshedEvent getActualEvent() {
			ContextRefreshedEvent localActualEvent = this.actualEvent;
			this.actualEvent = null;
			return localActualEvent;
		}

		public boolean isNotified() {
			boolean localNotified = this.notified;
			this.notified = false;
			return localNotified;
		}

		@Override
		public void onApplicationEvent(final ContextRefreshedEvent event) {
			this.actualEvent = event;
			this.notified = true;
		}

		@Override
		public String toString() {
			return this.name;
		}
	}

}
