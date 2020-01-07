/*
 * Copyright 2010-2020 the original author or authors.
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

package org.springframework.data.gemfire.support;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.CoreMatchers.isA;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import java.util.Arrays;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Matchers;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextException;
import org.springframework.context.ApplicationListener;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ApplicationContextEvent;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.ContextStartedEvent;
import org.springframework.context.event.ContextStoppedEvent;
import org.springframework.context.support.AbstractApplicationContext;
import org.springframework.util.ObjectUtils;

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

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	@After
	public void tearDown() {
		SpringContextBootstrappingInitializer.applicationContext = null;
		SpringContextBootstrappingInitializer.contextRefreshedEvent = null;
		SpringContextBootstrappingInitializer.setBeanClassLoader(null);
		SpringContextBootstrappingInitializer.unregister(TestAppConfigOne.class);
		SpringContextBootstrappingInitializer.unregister(TestAppConfigTwo.class);
	}

	protected static Properties createParameters(final String parameter, final String value) {
		Properties parameters = new Properties();
		parameters.setProperty(parameter, value);
		return parameters;
	}

	protected static Properties createParameters(final Properties parameters, final String parameter, final String value) {
		parameters.setProperty(parameter, value);
		return parameters;
	}

	@Test
	public void getInitializedApplicationContext() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"testGetApplicationContext");

		SpringContextBootstrappingInitializer.applicationContext = mockApplicationContext;

		assertThat(SpringContextBootstrappingInitializer.getApplicationContext(),
			is(sameInstance(mockApplicationContext)));
	}

	@Test
	public void getUninitializedApplicationContext() {
		expectedException.expect(IllegalStateException.class);
		expectedException.expectMessage("A Spring ApplicationContext was not configured and initialized properly");
		expectedException.expectCause(is(nullValue(Throwable.class)));

		SpringContextBootstrappingInitializer.getApplicationContext();
	}

	@Test
	public void setBeanClassLoaderWithCurrentThreadContextClassLoader() {
		assertThat(SpringContextBootstrappingInitializer.applicationContext, is(nullValue()));
		SpringContextBootstrappingInitializer.setBeanClassLoader(Thread.currentThread().getContextClassLoader());
	}

	@Test
	public void setBeanClassLoaderWithCurrentThreadContextClassLoaderWhenApplicationContextIsInactive() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"MockApplicationContext");

		when(mockApplicationContext.isActive()).thenReturn(false);

		SpringContextBootstrappingInitializer.applicationContext = mockApplicationContext;
		SpringContextBootstrappingInitializer.setBeanClassLoader(Thread.currentThread().getContextClassLoader());

		verify(mockApplicationContext, times(1)).isActive();
	}

	@Test
	public void setBeanClassLoaderWithCurrentThreadContextClassLoaderWhenApplicationContextIsActive() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"MockApplicationContext");

		when(mockApplicationContext.isActive()).thenReturn(true);

		expectedException.expect(IllegalStateException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("A Spring ApplicationContext has already been initialized");

		try {
			SpringContextBootstrappingInitializer.applicationContext = mockApplicationContext;
			SpringContextBootstrappingInitializer.setBeanClassLoader(Thread.currentThread().getContextClassLoader());
		}
		finally {
			verify(mockApplicationContext, times(1)).isActive();
		}
	}

	@Test
	public void createApplicationContextWhenAnnotatedClassesBasePackagesAndConfigLocationsAreUnspecified() {
		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("'AnnotatedClasses', 'basePackages' or 'configLocations' must be specified"
			+ " in order to construct and configure an instance of the ConfigurableApplicationContext");

		new SpringContextBootstrappingInitializer().createApplicationContext(null, null);
	}

	@Test
	public void createAnnotationApplicationContextWithAnnotatedClasses() {
		final AnnotationConfigApplicationContext mockAnnotationApplicationContext = mock(AnnotationConfigApplicationContext.class,
			"MockAnnotationApplicationContext");

		final ConfigurableApplicationContext mockXmlApplicationContext = mock(ConfigurableApplicationContext.class,
			"MockXmlApplicationContext");

		Class<?>[] annotatedClasses = { TestAppConfigOne.class, TestAppConfigTwo.class };

		SpringContextBootstrappingInitializer.register(annotatedClasses[0]);
		SpringContextBootstrappingInitializer.register(annotatedClasses[1]);

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {
			@Override ConfigurableApplicationContext createApplicationContext(final String[] configLocations) {
				return (ObjectUtils.isEmpty(configLocations) ? mockAnnotationApplicationContext
					: mockXmlApplicationContext);
			}
		};

		ConfigurableApplicationContext actualApplicationContext = initializer.createApplicationContext(null, null);

		assertThat(actualApplicationContext,
			is(sameInstance((ConfigurableApplicationContext) mockAnnotationApplicationContext)));

		verify(mockAnnotationApplicationContext, times(1)).register(annotatedClasses[0], annotatedClasses[1]);
	}

	@Test
	public void createAnnotationApplicationContextWithBasePackages() {
		final AnnotationConfigApplicationContext mockAnnotationApplicationContext = mock(AnnotationConfigApplicationContext.class,
			"MockAnnotationApplicationContext");

		final ConfigurableApplicationContext mockXmlApplicationContext = mock(ConfigurableApplicationContext.class,
			"MockXmlApplicationContext");

		String[] basePackages = { "org.example.app" };

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {
			@Override ConfigurableApplicationContext createApplicationContext(final String[] configLocations) {
				return (ObjectUtils.isEmpty(configLocations) ? mockAnnotationApplicationContext
					: mockXmlApplicationContext);
			}
		};

		ConfigurableApplicationContext actualApplicationContext = initializer.createApplicationContext(basePackages, null);

		assertThat(actualApplicationContext,
			is(sameInstance((ConfigurableApplicationContext) mockAnnotationApplicationContext)));

		verify(mockAnnotationApplicationContext, times(1)).scan(eq(basePackages[0]));
	}

	@Test
	public void createXmlApplicationContext() {
		final ConfigurableApplicationContext mockAnnotationApplicationContext = mock(ConfigurableApplicationContext.class,
			"MockAnnotationApplicationContext");

		final ConfigurableApplicationContext mockXmlApplicationContext = mock(ConfigurableApplicationContext.class,
			"MockXmlApplicationContext");

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {
			@Override ConfigurableApplicationContext createApplicationContext(final String[] configLocations) {
				return (ObjectUtils.isEmpty(configLocations) ? mockAnnotationApplicationContext
					: mockXmlApplicationContext);
			}
		};

		ConfigurableApplicationContext actualApplicationContext = initializer.createApplicationContext(null,
			new String[] { "/path/to/application/context.xml" });

		assertThat(actualApplicationContext, is(sameInstance(mockXmlApplicationContext)));
	}

	@Test
	public void initApplicationContext() {
		AbstractApplicationContext mockApplicationContext = mock(AbstractApplicationContext.class,
			"MockApplicationContext");

		SpringContextBootstrappingInitializer.setBeanClassLoader(Thread.currentThread().getContextClassLoader());

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer();

		assertSame(mockApplicationContext, initializer.initApplicationContext(mockApplicationContext));

		verify(mockApplicationContext, times(1)).addApplicationListener(same(initializer));
		verify(mockApplicationContext, times(1)).registerShutdownHook();
		verify(mockApplicationContext, times(1)).setClassLoader(eq(Thread.currentThread().getContextClassLoader()));
	}

	@Test
	public void initApplicationContextWithNull() {
		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("ConfigurableApplicationContext must not be null");

		new SpringContextBootstrappingInitializer().initApplicationContext(null);
	}

	@Test
	public void refreshApplicationContext() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"MockApplicationContext");

		assertThat(new SpringContextBootstrappingInitializer().refreshApplicationContext(mockApplicationContext),
			is(sameInstance(mockApplicationContext)));

		verify(mockApplicationContext, times(1)).refresh();
	}

	@Test
	public void refreshApplicationContextWithNull() {
		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("ConfigurableApplicationContext must not be null");

		new SpringContextBootstrappingInitializer().refreshApplicationContext(null);
	}

	@Test
	public void registerAnnotatedClasses() {
		AnnotationConfigApplicationContext mockApplicationContext = mock(AnnotationConfigApplicationContext.class,
			"MockApplicationContext");

		Class<?>[] annotatedClasses = { TestAppConfigOne.class, TestAppConfigTwo.class };

		assertThat(new SpringContextBootstrappingInitializer()
				.registerAnnotatedClasses(mockApplicationContext, annotatedClasses),
			is(sameInstance(mockApplicationContext)));

		verify(mockApplicationContext, times(1)).register(annotatedClasses);
	}

	@Test
	public void registerAnnotatedClassesWithEmptyAnnotatedClassesArray() {
		AnnotationConfigApplicationContext mockApplicationContext = mock(AnnotationConfigApplicationContext.class,
			"MockApplicationContext");

		assertThat(new SpringContextBootstrappingInitializer().registerAnnotatedClasses(mockApplicationContext,
				new Class<?>[0]),
			is(sameInstance(mockApplicationContext)));

		verify(mockApplicationContext, never()).register(any(Class[].class));
	}

	@Test
	public void registerAnnotatedClassesWithNonAnnotationBasedApplicationContext() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"MockApplicationContext");

		assertThat(new SpringContextBootstrappingInitializer().registerAnnotatedClasses(mockApplicationContext,
			new Class<?>[] { TestAppConfigOne.class }), is(sameInstance(mockApplicationContext)));
	}

	@Test
	public void scanBasePackages() {
		AnnotationConfigApplicationContext mockApplicationContext = mock(AnnotationConfigApplicationContext.class,
			"MockApplicationContext");

		String[] basePackages = { "org.example.app", "org.example.plugins" };

		assertThat(new SpringContextBootstrappingInitializer().scanBasePackages(mockApplicationContext, basePackages),
			is(sameInstance(mockApplicationContext)));

		verify(mockApplicationContext, times(1)).scan(basePackages);
	}

	@Test
	public void scanBasePackagesWithEmptyBasePackagesArray() {
		AnnotationConfigApplicationContext mockApplicationContext = mock(AnnotationConfigApplicationContext.class,
			"MockApplicationContext");

		assertThat(new SpringContextBootstrappingInitializer().scanBasePackages(mockApplicationContext, null),
			is(sameInstance(mockApplicationContext)));

		verify(mockApplicationContext, never()).scan(any(String[].class));
	}

	@Test
	public void scanBasePackagesWithNonAnnotationBasedApplicationContext() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"MockApplicationContext");

		assertThat(new SpringContextBootstrappingInitializer().scanBasePackages(mockApplicationContext,
			new String[] { "org.example.app" }), is(sameInstance(mockApplicationContext)));
	}

	@Test
	public void setClassLoader() {
		AbstractApplicationContext mockApplicationContext = mock(AbstractApplicationContext.class,
			"MockApplicationContext");

		SpringContextBootstrappingInitializer.setBeanClassLoader(Thread.currentThread().getContextClassLoader());

		assertThat(new SpringContextBootstrappingInitializer().setClassLoader(mockApplicationContext),
			is(sameInstance(mockApplicationContext)));

		verify(mockApplicationContext, times(1)).setClassLoader(eq(Thread.currentThread().getContextClassLoader()));
	}

	@Test
	public void setClassLoaderWithNonSettableClassLoaderApplicationContext() {
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class,
			"MockApplicationContext");

		SpringContextBootstrappingInitializer.setBeanClassLoader(Thread.currentThread().getContextClassLoader());

		assertThat(new SpringContextBootstrappingInitializer().setClassLoader(mockApplicationContext),
			is(sameInstance(mockApplicationContext)));
	}

	@Test
	public void setClassLoaderWithNullClassLoader() {
		AbstractApplicationContext mockApplicationContext = mock(AbstractApplicationContext.class,
			"MockApplicationContext");

		SpringContextBootstrappingInitializer.setBeanClassLoader(null);

		assertThat(new SpringContextBootstrappingInitializer().setClassLoader(mockApplicationContext),
			is(sameInstance(mockApplicationContext)));

		verify(mockApplicationContext, never()).setClassLoader(any(ClassLoader.class));
	}

	private Class<?>[] annotatedClasses(final Class<?>... annotatedClasses) {
		return argThat(argument -> {
			assertThat(argument instanceof Class<?>[], is(true));
			return Arrays.equals(annotatedClasses, (Class<?>[]) argument);
		});
	}

	@Test
	public void nullSafeGetApplicationContextIdWithNullReference() {
		assertThat(new SpringContextBootstrappingInitializer().nullSafeGetApplicationContextId(null), is(nullValue()));
	}

	@Test
	public void nullSafeGetApplicationContextIdWithNonNullReference() {
		ApplicationContext mockApplicationContext = mock(ApplicationContext.class, "MockApplicationContext");

		when(mockApplicationContext.getId()).thenReturn("123");

		assertThat(new SpringContextBootstrappingInitializer().nullSafeGetApplicationContextId(mockApplicationContext),
			is(equalTo("123")));
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

	@Test
	public void testInitWhenBasePackagesAndContextConfigLocationsParametersAreUnspecified() throws Throwable {
		assertThat(SpringContextBootstrappingInitializer.applicationContext, is(nullValue()));

		expectedException.expect(ApplicationContextException.class);
		expectedException.expectCause(isA(IllegalArgumentException.class));
		expectedException.expectMessage(containsString("Failed to bootstrap the Spring ApplicationContext"));

		new SpringContextBootstrappingInitializer().init(createParameters(createParameters(
				SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER, ""),
			SpringContextBootstrappingInitializer.BASE_PACKAGES_PARAMETER, "  "));
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
			assertTrue(expected.getMessage().contains("Failed to bootstrap the Spring ApplicationContext"));
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

			@Override protected ConfigurableApplicationContext createApplicationContext(String[] basePackages,
					String[] configLocations) {

				throw new IllegalStateException("TEST");
			}
		};

		try {
			initializer.init(createParameters(SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER,
				"classpath/to/spring/application/context.xml"));
		}
		catch (ApplicationContextException expected) {
			assertTrue(expected.getMessage().contains("Failed to bootstrap the Spring ApplicationContext"));
			assertTrue(expected.getCause() instanceof IllegalStateException);
			assertEquals("TEST", expected.getCause().getMessage());
			throw expected.getCause();
		}
		finally {
			verify(mockLog, times(1)).error(eq("Failed to bootstrap the Spring ApplicationContext"),
				any(RuntimeException.class));
		}
	}

	protected static void assertNotified(TestApplicationListener listener, ApplicationContextEvent expectedEvent) {
		assertThat(listener, is(notNullValue()));
		assertThat(listener.isNotified(), is(true));
		assertThat(listener.getActualEvent(), is(sameInstance(expectedEvent)));
	}

	protected static void assertUnnotified(TestApplicationListener listener) {
		assertThat(listener, is(notNullValue()));
		assertThat(listener.isNotified(), is(false));
		assertThat(listener.getActualEvent(), is(nullValue()));
	}

	@Test
	public void onContextClosedApplicationEvent() {
		TestApplicationListener testApplicationListener = new TestApplicationListener(
			"testOnContextClosedApplicationEvent");

		try {
			testApplicationListener = SpringContextBootstrappingInitializer.register(testApplicationListener);

			assertUnnotified(testApplicationListener);

			SpringContextBootstrappingInitializer.contextRefreshedEvent = mock(ContextRefreshedEvent.class,
				"MockContextRefreshedEvent");

			assertThat(SpringContextBootstrappingInitializer.contextRefreshedEvent, isA(ContextRefreshedEvent.class));

			new SpringContextBootstrappingInitializer().onApplicationEvent(mock(ContextClosedEvent.class,
				"MockContextClosedEvent"));

			assertThat(SpringContextBootstrappingInitializer.contextRefreshedEvent, is(nullValue()));
			assertUnnotified(testApplicationListener);
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testApplicationListener);
		}
	}

	@Test
	public void onContextRefreshedApplicationEvent() {
		TestApplicationListener testApplicationListener = new TestApplicationListener(
			"testOnContextRefreshedApplicationEvent");

		try {
			testApplicationListener = SpringContextBootstrappingInitializer.register(testApplicationListener);

			assertThat(SpringContextBootstrappingInitializer.contextRefreshedEvent, is(nullValue()));
			assertUnnotified(testApplicationListener);

			ContextRefreshedEvent mockContextRefreshedEvent = mock(ContextRefreshedEvent.class,
				"MockContextRefreshedEvent");

			new SpringContextBootstrappingInitializer().onApplicationEvent(mockContextRefreshedEvent);

			assertThat(SpringContextBootstrappingInitializer.contextRefreshedEvent, is(sameInstance(mockContextRefreshedEvent)));
			assertNotified(testApplicationListener, mockContextRefreshedEvent);
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testApplicationListener);
		}
	}

	@Test
	public void onContextStartedApplicationEvent() {
		TestApplicationListener testApplicationListener = new TestApplicationListener(
			"testOnContextStartedApplicationEvent");

		try {
			testApplicationListener = SpringContextBootstrappingInitializer.register(testApplicationListener);

			assertThat(SpringContextBootstrappingInitializer.contextRefreshedEvent, is(nullValue()));
			assertUnnotified(testApplicationListener);

			new SpringContextBootstrappingInitializer().onApplicationEvent(mock(ContextStartedEvent.class,
				"MockContextStartedEvent"));

			assertThat(SpringContextBootstrappingInitializer.contextRefreshedEvent, is(nullValue()));
			assertUnnotified(testApplicationListener);
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testApplicationListener);
		}
	}

	@Test
	public void onContextStoppedApplicationEvent() {
		TestApplicationListener testApplicationListener = new TestApplicationListener(
			"testOnContextStartedApplicationEvent");

		try {
			testApplicationListener = SpringContextBootstrappingInitializer.register(testApplicationListener);

			assertUnnotified(testApplicationListener);

			ContextRefreshedEvent mockContextRefreshedEvent = mock(ContextRefreshedEvent.class,
				"MockContextRefreshedEvent");

			SpringContextBootstrappingInitializer.contextRefreshedEvent = mockContextRefreshedEvent;

			assertThat(SpringContextBootstrappingInitializer.contextRefreshedEvent, is(sameInstance(
				mockContextRefreshedEvent)));

			new SpringContextBootstrappingInitializer().onApplicationEvent(mock(ContextStoppedEvent.class,
				"MockContextStoppedEvent"));

			assertThat(SpringContextBootstrappingInitializer.contextRefreshedEvent, is(sameInstance(mockContextRefreshedEvent)));
			assertUnnotified(testApplicationListener);
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testApplicationListener);
		}
	}

	@Test
	public void onApplicationEventWithMultipleRegisteredApplicationListeners() {
		TestApplicationListener testApplicationListenerOne = new TestApplicationListener("TestApplicationListener.1");

		TestApplicationListener testApplicationListenerTwo = new TestApplicationListener("TestApplicationListener.2");

		TestApplicationListener testApplicationListenerThree = new TestApplicationListener("TestApplicationListener.3");

		try {
			testApplicationListenerOne = SpringContextBootstrappingInitializer.register(testApplicationListenerOne);
			testApplicationListenerTwo = SpringContextBootstrappingInitializer.register(testApplicationListenerTwo);
			testApplicationListenerThree = SpringContextBootstrappingInitializer.register(testApplicationListenerThree);

			assertThat(SpringContextBootstrappingInitializer.contextRefreshedEvent, is(nullValue()));
			assertUnnotified(testApplicationListenerOne);
			assertUnnotified(testApplicationListenerTwo);
			assertUnnotified(testApplicationListenerThree);

			ContextRefreshedEvent mockContextRefreshedEvent = mock(ContextRefreshedEvent.class,
				"MockContextRefreshedEvent");

			new SpringContextBootstrappingInitializer().onApplicationEvent(mockContextRefreshedEvent);

			assertThat(SpringContextBootstrappingInitializer.contextRefreshedEvent,
				is(sameInstance(mockContextRefreshedEvent)));
			assertNotified(testApplicationListenerOne, mockContextRefreshedEvent);
			assertNotified(testApplicationListenerTwo, mockContextRefreshedEvent);
			assertNotified(testApplicationListenerThree, mockContextRefreshedEvent);
		}
		finally {
			SpringContextBootstrappingInitializer.unregister(testApplicationListenerOne);
			SpringContextBootstrappingInitializer.unregister(testApplicationListenerTwo);
			SpringContextBootstrappingInitializer.unregister(testApplicationListenerThree);
		}
	}

	@Test
	public void onApplicationEventWithNoRegisteredApplicationListener() {
		TestApplicationListener testApplicationListener = new TestApplicationListener("TestApplicationListener");

		try {
			testApplicationListener = SpringContextBootstrappingInitializer.unregister(
				SpringContextBootstrappingInitializer.register(testApplicationListener));

			assertUnnotified(testApplicationListener);

			new SpringContextBootstrappingInitializer().onApplicationEvent(mock(ContextRefreshedEvent.class,
				"MockContextRefreshedEvent"));

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

		private volatile ApplicationContextEvent actualEvent;

		private final String name;

		public TestApplicationListener(final String name) {
			this.name = name;
		}

		public ApplicationContextEvent getActualEvent() {
			ApplicationContextEvent localActualEvent = this.actualEvent;
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
