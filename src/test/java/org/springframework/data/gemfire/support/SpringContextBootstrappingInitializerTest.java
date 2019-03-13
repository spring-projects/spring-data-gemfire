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

package org.springframework.data.gemfire.support;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.isA;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.geode.cache.Cache;
import org.junit.After;
import org.junit.Test;
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

	private static Properties createParameters(String parameter, String value) {

		Properties parameters = new Properties();

		parameters.setProperty(parameter, value);

		return parameters;
	}

	private static Properties createParameters(Properties parameters, String parameter, String value) {

		parameters.setProperty(parameter, value);

		return parameters;
	}

	private Cache mockCache = mock(Cache.class);

	@After
	public void tearDown() {

		SpringContextBootstrappingInitializer.applicationContext = null;
		SpringContextBootstrappingInitializer.contextRefreshedEvent = null;
		SpringContextBootstrappingInitializer.setBeanClassLoader(null);
		SpringContextBootstrappingInitializer.unregister(TestAppConfigOne.class);
		SpringContextBootstrappingInitializer.unregister(TestAppConfigTwo.class);
	}

	@Test
	public void getInitializedApplicationContext() {

		ConfigurableApplicationContext mockApplicationContext =
			mock(ConfigurableApplicationContext.class, "testGetApplicationContext");

		SpringContextBootstrappingInitializer.applicationContext = mockApplicationContext;

		assertThat(SpringContextBootstrappingInitializer.getApplicationContext(),
			is(sameInstance(mockApplicationContext)));
	}

	@Test(expected = IllegalStateException.class)
	public void getUninitializedApplicationContext() {

		try {
			SpringContextBootstrappingInitializer.getApplicationContext();
		}
		catch (IllegalStateException expected) {

			assertThat(expected.getMessage(),
				containsString("A Spring ApplicationContext was not configured and initialized properly"));

			assertThat(expected.getCause(), is(nullValue(Throwable.class)));

			throw expected;
		}
	}

	@Test
	public void setBeanClassLoaderWithCurrentThreadContextClassLoader() {

		assertThat(SpringContextBootstrappingInitializer.applicationContext, is(nullValue()));

		SpringContextBootstrappingInitializer.setBeanClassLoader(Thread.currentThread().getContextClassLoader());
	}

	@Test
	public void setBeanClassLoaderWithCurrentThreadContextClassLoaderWhenApplicationContextIsInactive() {

		ConfigurableApplicationContext mockApplicationContext =
			mock(ConfigurableApplicationContext.class,"MockApplicationContext");

		when(mockApplicationContext.isActive()).thenReturn(false);

		SpringContextBootstrappingInitializer.applicationContext = mockApplicationContext;
		SpringContextBootstrappingInitializer.setBeanClassLoader(Thread.currentThread().getContextClassLoader());

		verify(mockApplicationContext, times(1)).isActive();
	}

	@Test(expected = IllegalStateException.class)
	public void setBeanClassLoaderWithCurrentThreadContextClassLoaderWhenApplicationContextIsActive() {

		ConfigurableApplicationContext mockApplicationContext =
			mock(ConfigurableApplicationContext.class,"MockApplicationContext");

		when(mockApplicationContext.isActive()).thenReturn(true);

		try {
			SpringContextBootstrappingInitializer.applicationContext = mockApplicationContext;
			SpringContextBootstrappingInitializer.setBeanClassLoader(Thread.currentThread().getContextClassLoader());
		}
		catch (IllegalStateException expected) {

			assertThat(expected.getMessage(),
				containsString("A Spring ApplicationContext has already been initialized"));

			assertThat(expected.getCause(), is(nullValue(Throwable.class)));

			throw expected;
		}
		finally {
			verify(mockApplicationContext, times(1)).isActive();
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void createApplicationContextWhenAnnotatedClassesBasePackagesAndConfigLocationsAreUnspecified() {

		try {
			new SpringContextBootstrappingInitializer().createApplicationContext(null, null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected.getMessage(),
				containsString("'AnnotatedClasses', 'basePackages' or 'configLocations' must be specified"
					+ " in order to construct and configure an instance of the ConfigurableApplicationContext"));

			assertThat(expected.getCause(), is(nullValue(Throwable.class)));

			throw expected;
		}
	}

	@Test
	public void createAnnotationBasedApplicationContextWithAnnotatedClasses() {

		AnnotationConfigApplicationContext mockAnnotationApplicationContext =
			mock(AnnotationConfigApplicationContext.class, "MockAnnotationApplicationContext");

		ConfigurableApplicationContext mockXmlApplicationContext =
			mock(ConfigurableApplicationContext.class, "MockXmlApplicationContext");

		Class<?>[] annotatedClasses = { TestAppConfigOne.class, TestAppConfigTwo.class };

		Arrays.stream(annotatedClasses).forEach(SpringContextBootstrappingInitializer::register);

		SpringContextBootstrappingInitializer initializer = spy(new SpringContextBootstrappingInitializer() {

			@Override
			ConfigurableApplicationContext createApplicationContext(String[] configLocations) {

				return ObjectUtils.isEmpty(configLocations)
					? mockAnnotationApplicationContext
					: mockXmlApplicationContext;
			}
		});

		/*
		doAnswer(invocationOnMock ->
			ObjectUtils.isEmpty(invocationOnMock.getArgument(0))
				? mockAnnotationApplicationContext
				: mockXmlApplicationContext
		).when(initializer).createApplicationContext(any(String[].class));
		*/

		doReturn(mockAnnotationApplicationContext)
			.when(initializer).doRegister(any(ConfigurableApplicationContext.class), any(Class[].class));

		ConfigurableApplicationContext actualApplicationContext =
			initializer.createApplicationContext(null, null);

		assertThat(actualApplicationContext, is(sameInstance(mockAnnotationApplicationContext)));

		verify(initializer, times(1))
			.doRegister(eq(mockAnnotationApplicationContext), eq(annotatedClasses));

		verifyZeroInteractions(mockXmlApplicationContext);
	}

	@Test
	public void createAnnotationBasedApplicationContextWithBasePackages() {

		AnnotationConfigApplicationContext mockAnnotationApplicationContext =
			mock(AnnotationConfigApplicationContext.class,"MockAnnotationApplicationContext");

		ConfigurableApplicationContext mockXmlApplicationContext =
			mock(ConfigurableApplicationContext.class,"MockXmlApplicationContext");

		String[] basePackages = { "org.example.app" };

		SpringContextBootstrappingInitializer initializer = spy(new SpringContextBootstrappingInitializer() {

			@Override
			ConfigurableApplicationContext createApplicationContext(String[] configLocations) {

				return ObjectUtils.isEmpty(configLocations)
					? mockAnnotationApplicationContext
					: mockXmlApplicationContext;
			}
		});

		doReturn(mockAnnotationApplicationContext)
			.when(initializer).doScan(any(ConfigurableApplicationContext.class), any(String[].class));

		ConfigurableApplicationContext actualApplicationContext =
			initializer.createApplicationContext(basePackages, null);

		assertThat(actualApplicationContext, is(sameInstance(mockAnnotationApplicationContext)));

		verify(initializer, times(1))
			.scanBasePackages(eq(mockAnnotationApplicationContext), eq(basePackages));
	}

	@Test
	public void createXmlBasedApplicationContext() {

		ConfigurableApplicationContext mockAnnotationApplicationContext =
			mock(ConfigurableApplicationContext.class,"MockAnnotationApplicationContext");

		ConfigurableApplicationContext mockXmlApplicationContext =
			mock(ConfigurableApplicationContext.class,"MockXmlApplicationContext");

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {

			@Override
			ConfigurableApplicationContext createApplicationContext(final String[] configLocations) {

				return ObjectUtils.isEmpty(configLocations)
					? mockAnnotationApplicationContext
					: mockXmlApplicationContext;
			}
		};

		ConfigurableApplicationContext actualApplicationContext =
			initializer.createApplicationContext(null, new String[] { "/path/to/application/context.xml" });

		assertThat(actualApplicationContext, is(sameInstance(mockXmlApplicationContext)));
	}

	@Test
	public void initApplicationContext() {

		AbstractApplicationContext mockApplicationContext =
			mock(AbstractApplicationContext.class,"MockApplicationContext");

		SpringContextBootstrappingInitializer.setBeanClassLoader(Thread.currentThread().getContextClassLoader());

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer();

		assertSame(mockApplicationContext, initializer.initApplicationContext(mockApplicationContext));

		verify(mockApplicationContext, times(1)).addApplicationListener(same(initializer));
		verify(mockApplicationContext, times(1)).registerShutdownHook();
		verify(mockApplicationContext, times(1)).setClassLoader(eq(Thread.currentThread().getContextClassLoader()));
	}

	@Test(expected = IllegalArgumentException.class)
	public void initApplicationContextWithNull() {

		try {
			new SpringContextBootstrappingInitializer().initApplicationContext(null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected.getMessage(),
				containsString("ConfigurableApplicationContext must not be null"));

			assertThat(expected.getCause(), is(nullValue(Throwable.class)));

			throw expected;
		}
	}

	@Test
	public void refreshApplicationContext() {

		ConfigurableApplicationContext mockApplicationContext =
			mock(ConfigurableApplicationContext.class,"MockApplicationContext");

		assertThat(new SpringContextBootstrappingInitializer().refreshApplicationContext(mockApplicationContext),
			is(sameInstance(mockApplicationContext)));

		verify(mockApplicationContext, times(1)).refresh();
	}

	@Test(expected = IllegalArgumentException.class)
	public void refreshApplicationContextWithNull() {

		try {
			new SpringContextBootstrappingInitializer().refreshApplicationContext(null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected.getMessage(),
				containsString("ConfigurableApplicationContext must not be null"));

			assertThat(expected.getCause(), is(nullValue(Throwable.class)));

			throw expected;
		}
	}

	@Test
	public void registerAnnotatedClasses() {

		AnnotationConfigApplicationContext mockApplicationContext =
			mock(AnnotationConfigApplicationContext.class,"MockApplicationContext");

		Class<?>[] annotatedClasses = { TestAppConfigOne.class, TestAppConfigTwo.class };

		SpringContextBootstrappingInitializer initializer = spy(new SpringContextBootstrappingInitializer());

		doReturn(mockApplicationContext)
			.when(initializer).doRegister(any(ConfigurableApplicationContext.class), any(Class[].class));

		assertThat(initializer.registerAnnotatedClasses(mockApplicationContext, annotatedClasses),
			is(sameInstance(mockApplicationContext)));

		verify(initializer, times(1))
			.doRegister(eq(mockApplicationContext), eq(annotatedClasses));
	}

	@Test
	public void registerAnnotatedClassesWithEmptyAnnotatedClassesArray() {

		AnnotationConfigApplicationContext mockApplicationContext =
			mock(AnnotationConfigApplicationContext.class, "MockApplicationContext");

		SpringContextBootstrappingInitializer initializer = spy(new SpringContextBootstrappingInitializer());

		doReturn(mockApplicationContext)
			.when(initializer).doRegister(any(ConfigurableApplicationContext.class), any(Class[].class));

		assertThat(initializer.registerAnnotatedClasses(mockApplicationContext, new Class<?>[0]),
			is(sameInstance(mockApplicationContext)));

		verify(initializer, never()).doRegister(any(ConfigurableApplicationContext.class), any(Class[].class));
	}

	@Test
	public void registerAnnotatedClassesWithNonAnnotationBasedApplicationContext() {

		ConfigurableApplicationContext mockApplicationContext =
			mock(ConfigurableApplicationContext.class, "MockApplicationContext");

		SpringContextBootstrappingInitializer initializer = spy(new SpringContextBootstrappingInitializer());

		doReturn(mockApplicationContext)
			.when(initializer).doRegister(any(ConfigurableApplicationContext.class), any(Class[].class));

		assertThat(initializer.registerAnnotatedClasses(mockApplicationContext, new Class<?>[] { TestAppConfigOne.class }),
			is(sameInstance(mockApplicationContext)));

		verify(initializer, never()).doRegister(any(ConfigurableApplicationContext.class), any(Class[].class));
	}

	@Test
	public void scanBasePackages() {

		AnnotationConfigApplicationContext mockApplicationContext =
			mock(AnnotationConfigApplicationContext.class, "MockApplicationContext");

		SpringContextBootstrappingInitializer initializer = spy(new SpringContextBootstrappingInitializer());

		doReturn(mockApplicationContext)
			.when(initializer).doScan(any(ConfigurableApplicationContext.class), any(String[].class));

		String[] basePackages = { "org.example.app", "org.example.plugins" };

		assertThat(initializer.scanBasePackages(mockApplicationContext, basePackages),
			is(sameInstance(mockApplicationContext)));

		verify(initializer, times(1)).doScan(eq(mockApplicationContext), eq(basePackages));
	}

	@Test
	public void scanBasePackagesWithEmptyBasePackagesArray() {

		AnnotationConfigApplicationContext mockApplicationContext =
			mock(AnnotationConfigApplicationContext.class, "MockApplicationContext");

		SpringContextBootstrappingInitializer initializer = spy(new SpringContextBootstrappingInitializer());

		doReturn(mockApplicationContext)
			.when(initializer).doScan(any(ConfigurableApplicationContext.class), any(String[].class));

		assertThat(initializer.scanBasePackages(mockApplicationContext, null),
			is(sameInstance(mockApplicationContext)));

		verify(initializer, never()).doScan(any(ConfigurableApplicationContext.class), any(String[].class));
	}

	@Test
	public void scanBasePackagesWithNonAnnotationBasedApplicationContext() {

		ConfigurableApplicationContext mockApplicationContext =
			mock(ConfigurableApplicationContext.class, "MockApplicationContext");

		SpringContextBootstrappingInitializer initializer = spy(new SpringContextBootstrappingInitializer());

		doReturn(mockApplicationContext)
			.when(initializer).doScan(any(ConfigurableApplicationContext.class), any(String[].class));

		assertThat(initializer.scanBasePackages(mockApplicationContext, new String[] { "org.example.app" }),
			is(sameInstance(mockApplicationContext)));

		verify(initializer, never()).doScan(any(ConfigurableApplicationContext.class), any(String[].class));
	}

	@Test
	public void setClassLoader() {

		AbstractApplicationContext mockApplicationContext =
			mock(AbstractApplicationContext.class, "MockApplicationContext");

		SpringContextBootstrappingInitializer.setBeanClassLoader(Thread.currentThread().getContextClassLoader());

		assertThat(new SpringContextBootstrappingInitializer().setClassLoader(mockApplicationContext),
			is(sameInstance(mockApplicationContext)));

		verify(mockApplicationContext, times(1))
			.setClassLoader(eq(Thread.currentThread().getContextClassLoader()));
	}

	@Test
	public void setClassLoaderWithNonSettableClassLoaderApplicationContext() {

		ConfigurableApplicationContext mockApplicationContext =
			mock(ConfigurableApplicationContext.class, "MockApplicationContext");

		SpringContextBootstrappingInitializer.setBeanClassLoader(Thread.currentThread().getContextClassLoader());

		assertThat(new SpringContextBootstrappingInitializer().setClassLoader(mockApplicationContext),
			is(sameInstance(mockApplicationContext)));
	}

	@Test
	public void setClassLoaderWithNullClassLoader() {

		AbstractApplicationContext mockApplicationContext =
			mock(AbstractApplicationContext.class, "MockApplicationContext");

		SpringContextBootstrappingInitializer.setBeanClassLoader(null);

		assertThat(new SpringContextBootstrappingInitializer().setClassLoader(mockApplicationContext),
			is(sameInstance(mockApplicationContext)));

		verify(mockApplicationContext, never()).setClassLoader(any(ClassLoader.class));
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

		AnnotationConfigApplicationContext mockApplicationContext =
			mock(AnnotationConfigApplicationContext.class, "testInitWithAnnotatedClasses");

		doNothing().when(mockApplicationContext).addApplicationListener(any(ApplicationListener.class));
		doNothing().when(mockApplicationContext).registerShutdownHook();
		doNothing().when(mockApplicationContext).refresh();

		when(mockApplicationContext.getId()).thenReturn("testInitWithAnnotatedClasses");
		when(mockApplicationContext.isRunning()).thenReturn(true);

		assertNull(SpringContextBootstrappingInitializer.applicationContext);

		SpringContextBootstrappingInitializer.register(TestAppConfigOne.class);
		SpringContextBootstrappingInitializer.register(TestAppConfigTwo.class);

		SpringContextBootstrappingInitializer initializer = spy(new SpringContextBootstrappingInitializer() {

			@Override
			protected ConfigurableApplicationContext createApplicationContext(String[] configLocations) {
				return mockApplicationContext;
			}
		});

		doReturn(mockApplicationContext)
			.when(initializer).doRegister(any(ConfigurableApplicationContext.class), any(Class[].class));

		initializer.init(this.mockCache, createParameters("test", "test"));

		verify(mockApplicationContext, times(1)).addApplicationListener(same(initializer));
		verify(mockApplicationContext, times(1)).registerShutdownHook();
		verify(initializer, never()).doScan(any(ConfigurableApplicationContext.class), any(String[].class));
		verify(initializer, times(1)).doRegister(eq(mockApplicationContext), eq(new Class[] {
			TestAppConfigOne.class, TestAppConfigTwo.class }));

		assertEquals(mockApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());
	}

	@Test
	public void testInitWithExistingApplicationContext() {

		ConfigurableApplicationContext mockApplicationContext =
			mock(ConfigurableApplicationContext.class, "testInitWithExistingApplicationContext");

		when(mockApplicationContext.isActive()).thenReturn(true);
		when(mockApplicationContext.getId()).thenReturn("testInitWithExistingApplicationContext");

		SpringContextBootstrappingInitializer.applicationContext = mockApplicationContext;

		assertSame(mockApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer();

		initializer.init(this.mockCache, createParameters("test", "test"));

		verify(mockApplicationContext, never()).addApplicationListener(any(SpringContextBootstrappingInitializer.class));
		verify(mockApplicationContext, never()).registerShutdownHook();
		verify(mockApplicationContext, never()).refresh();

		assertSame(mockApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());
	}

	@Test
	public void testInitWhenApplicationContextIsNull() {

		assertNull(SpringContextBootstrappingInitializer.applicationContext);

		ConfigurableApplicationContext mockApplicationContext =
			mock(ConfigurableApplicationContext.class, "testInitWhenApplicationContextIsNull");

		when(mockApplicationContext.getId()).thenReturn("testInitWhenApplicationContextIsNull");
		when(mockApplicationContext.isRunning()).thenReturn(true);

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {

			@Override
			protected ConfigurableApplicationContext createApplicationContext(String[] basePackages,
					String[] configLocations) {

				return mockApplicationContext;
			}
		};

		Properties parameters = createParameters(SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER,
			"/path/to/spring/application/context.xml");

		initializer.init(this.mockCache, parameters);

		verify(mockApplicationContext, times(1)).addApplicationListener(same(initializer));
		verify(mockApplicationContext, times(1)).registerShutdownHook();
		verify(mockApplicationContext, times(1)).refresh();

		assertSame(mockApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());
	}

	@Test
	public void testInitWhenApplicationContextIsInactive() {

		ConfigurableApplicationContext mockInactiveApplicationContext =
			mock(ConfigurableApplicationContext.class, "testInitWhenApplicationContextIsInactive.Inactive");

		when(mockInactiveApplicationContext.isActive()).thenReturn(false);

		SpringContextBootstrappingInitializer.applicationContext = mockInactiveApplicationContext;

		assertSame(mockInactiveApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());

		final ConfigurableApplicationContext mockNewApplicationContext = mock(ConfigurableApplicationContext.class,
			"testInitWhenApplicationContextIsInactive.New");

		when(mockNewApplicationContext.getId()).thenReturn("testInitWhenApplicationContextIsInactive.New");
		when(mockNewApplicationContext.isRunning()).thenReturn(true);

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {

			@Override
			protected ConfigurableApplicationContext createApplicationContext(String[] basePackages,
					String[] configLocations) {

				return mockNewApplicationContext;
			}
		};

		initializer.init(this.mockCache, createParameters(SpringContextBootstrappingInitializer.BASE_PACKAGES_PARAMETER,
			"org.example.app"));

		verify(mockNewApplicationContext, times(1)).addApplicationListener(same(initializer));
		verify(mockNewApplicationContext, times(1)).registerShutdownHook();
		verify(mockNewApplicationContext, times(1)).refresh();

		assertSame(mockNewApplicationContext, SpringContextBootstrappingInitializer.getApplicationContext());
	}

	@Test(expected = ApplicationContextException.class)
	public void testInitWhenBasePackagesAndContextConfigLocationsParametersAreUnspecified() {

		assertThat(SpringContextBootstrappingInitializer.applicationContext, is(nullValue()));

		try {

			Properties parameters = createParameters(createParameters(
				SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER, ""),
				SpringContextBootstrappingInitializer.BASE_PACKAGES_PARAMETER, "  ");

			new SpringContextBootstrappingInitializer().init(this.mockCache, parameters);
		}
		catch (ApplicationContextException expected) {

			assertThat(expected.getMessage(), containsString("Failed to bootstrap the Spring ApplicationContext"));
			assertThat(expected.getCause(), is(instanceOf(IllegalArgumentException.class)));

			throw expected;
		}
	}

	@Test(expected = IllegalStateException.class)
	public void testInitWhenApplicationContextIsNotRunning() {
		assertNull(SpringContextBootstrappingInitializer.applicationContext);

		ConfigurableApplicationContext mockApplicationContext =
			mock(ConfigurableApplicationContext.class,"testInitWhenApplicationContextIsNotRunning");

		when(mockApplicationContext.getId()).thenReturn("testInitWhenApplicationContextIsNotRunning");
		when(mockApplicationContext.isRunning()).thenReturn(false);

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {

			@Override
			protected ConfigurableApplicationContext createApplicationContext(String[] basePackages,
					String[] configLocations) {

				return mockApplicationContext;
			}
		};

		try {

			Properties parameters = createParameters(SpringContextBootstrappingInitializer.BASE_PACKAGES_PARAMETER,
				"org.example.app, org.example.plugins");

			initializer.init(this.mockCache, parameters);

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

	@SuppressWarnings("all")
	@Test(expected = IllegalStateException.class)
	public void testInitLogsErrors() throws Throwable {

		Log mockLog = mock(Log.class, "testInitLogsErrors.MockLog");

		SpringContextBootstrappingInitializer initializer = new SpringContextBootstrappingInitializer() {

			@Override
			protected Log initLogger() {
				return mockLog;
			}

			@Override
			protected ConfigurableApplicationContext createApplicationContext(String[] basePackages,
					String[] configLocations) {

				throw new IllegalStateException("TEST");
			}
		};

		try {

			Properties parameters = createParameters(SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER,
				"classpath/to/spring/application/context.xml");

			initializer.init(this.mockCache, parameters);
		}
		catch (ApplicationContextException expected) {

			assertTrue(expected.getMessage().contains("Failed to bootstrap the Spring ApplicationContext"));
			assertTrue(expected.getCause() instanceof IllegalStateException);
			assertEquals("TEST", expected.getCause().getMessage());

			throw expected.getCause();
		}
		finally {
			verify(mockLog, times(1))
				.error(eq("Failed to bootstrap the Spring ApplicationContext"), any(RuntimeException.class));
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
	@SuppressWarnings("all")
	public void onContextClosedApplicationEvent() {

		TestApplicationListener testApplicationListener =
			new TestApplicationListener("testOnContextClosedApplicationEvent");

		try {

			testApplicationListener = SpringContextBootstrappingInitializer.register(testApplicationListener);

			assertUnnotified(testApplicationListener);

			SpringContextBootstrappingInitializer.contextRefreshedEvent =
				mock(ContextRefreshedEvent.class,"MockContextRefreshedEvent");

			assertThat(SpringContextBootstrappingInitializer.contextRefreshedEvent, isA(ContextRefreshedEvent.class));

			new SpringContextBootstrappingInitializer()
				.onApplicationEvent(mock(ContextClosedEvent.class,"MockContextClosedEvent"));

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
