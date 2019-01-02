/*
 * Copyright 2017-2019 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.annotation.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.util.Arrays;
import java.util.Hashtable;
import java.util.List;
import java.util.Optional;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import org.apache.geode.cache.GemFireCache;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentMatcher;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.internal.matchers.VarargMatcher;
import org.mockito.junit.MockitoJUnitRunner;
import org.slf4j.Logger;

/**
 * Unit tests for {@link AbstractGemFireAsLastResourceAspectSupport}.
 *
 * @author John Blum
 * @see javax.naming.Context
 * @see javax.naming.InitialContext
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.apache.geode.cache.GemFireCache
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractGemFireAsLastResourceAspectSupport
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class AbstractGemFireAsLastResourceAspectSupportUnitTests {

	@Mock
	private Logger mockLogger;

	@Spy
	private AbstractGemFireAsLastResourceAspectSupport aspectSupport;

	@Before
	@SuppressWarnings("all")
	public void setup() {
		doReturn(mockLogger).when(aspectSupport).getLogger();
	}

	@Test
	public void setAndGetOrder() {

		assertThat(aspectSupport.getOrder()).isEqualTo(AbstractGemFireAsLastResourceAspectSupport.DEFAULT_ORDER);

		aspectSupport.setOrder(1);

		assertThat(aspectSupport.getOrder()).isEqualTo(1);

		aspectSupport.setOrder(-2);

		assertThat(aspectSupport.getOrder()).isEqualTo(-2);

		aspectSupport.setOrder(Integer.MAX_VALUE);

		assertThat(aspectSupport.getOrder()).isEqualTo(AbstractGemFireAsLastResourceAspectSupport.DEFAULT_ORDER);

		aspectSupport.setOrder(Integer.MIN_VALUE);

		assertThat(aspectSupport.getOrder()).isEqualTo(AbstractGemFireAsLastResourceAspectSupport.DEFAULT_ORDER);
	}

	@Test
	public void logDebugInfoWhenDebuggingIsDisabled() {

		when(mockLogger.isDebugEnabled()).thenReturn(false);

		assertThat(aspectSupport.<AbstractGemFireAsLastResourceAspectSupport>logDebugInfo("message", "arg"))
			.isSameAs(aspectSupport);

		verify(mockLogger, times(1)).isDebugEnabled();
		verify(mockLogger, never()).debug(anyString());
		verify(mockLogger, never()).debug(anyString(), any(Object.class));
		verify(mockLogger, never()).debug(anyString(), any(Object.class), any(Object.class));
		verify(mockLogger, never()).debug(anyString(), any(Object[].class));
	}

	@Test
	public void logDebugInfoWhenDebuggingIsEnabled() {

		when(mockLogger.isDebugEnabled()).thenReturn(true);

		assertThat(aspectSupport.<AbstractGemFireAsLastResourceAspectSupport>logDebugInfo("test %s message",
			"debug", "test")) .isSameAs(aspectSupport);

		verify(mockLogger, times(1)).isDebugEnabled();
		// TODO why the f#&k does this not work Mockito?!
		//verify(mockLogger, times(1))
		//	.debug(eq("test debug message"), eq("debug"), eq("test"));
		// TODO this ridiculous sh!t works
		//verify(mockLogger, times(1)).debug(eq("test debug message"),
		//	ArgumentMatchers.<Object[]>any());
		// TODO and so does this, but what a hack!
		verify(mockLogger, times(1)).debug(eq("test debug message"),
			VariableArgumentMatcher.varArgThat("debug", "test"));
	}

	@Test
	public void logInfoWhenInfoIsDisabled() {

		when(mockLogger.isInfoEnabled()).thenReturn(false);

		assertThat(aspectSupport.<AbstractGemFireAsLastResourceAspectSupport>logInfo("message", "arg"))
			.isSameAs(aspectSupport);

		verify(mockLogger, times(1)).isInfoEnabled();
		verify(mockLogger, never()).info(anyString(), any(Object.class));
		verify(mockLogger, never()).info(anyString(), any(Object.class), any(Object.class));
		verify(mockLogger, never()).info(anyString(), any(Object[].class));
	}

	@Test
	public void logInfoWhenInfoIsEnabled() {

		when(mockLogger.isInfoEnabled()).thenReturn(true);

		assertThat(aspectSupport.<AbstractGemFireAsLastResourceAspectSupport>logInfo("test %s message", "info"))
			.isSameAs(aspectSupport);

		verify(mockLogger, times(1)).isInfoEnabled();
		verify(mockLogger, times(1)).info(eq("test info message"),
			VariableArgumentMatcher.varArgThat("info"));
	}

	@Test
	public void logTraceInfoWhenTracingIsDisabled() {

		when(mockLogger.isTraceEnabled()).thenReturn(false);

		assertThat(aspectSupport.<AbstractGemFireAsLastResourceAspectSupport>logTraceInfo("message", "arg"))
			.isSameAs(aspectSupport);

		verify(mockLogger, times(1)).isTraceEnabled();
		verify(mockLogger, never()).trace(anyString());
		verify(mockLogger, never()).trace(anyString(), any(Object.class));
		verify(mockLogger, never()).trace(anyString(), any(Object.class), any(Object.class));
		verify(mockLogger, never()).trace(anyString(), any(Object[].class));
	}

	@Test
	public void logTraceInfoWhenTracingIsEnabled() {

		when(mockLogger.isTraceEnabled()).thenReturn(true);

		assertThat(aspectSupport.<AbstractGemFireAsLastResourceAspectSupport>logTraceInfo("test %s message", "trace"))
			.isSameAs(aspectSupport);

		verify(mockLogger, times(1)).isTraceEnabled();
		verify(mockLogger, times(1)).trace(eq("test trace message"));
	}

	@Test
	public void logTraceInfoUsingMessageSupplierWhenTracingIsEnabled() {

		when(mockLogger.isTraceEnabled()).thenReturn(true);

		assertThat(aspectSupport.<AbstractGemFireAsLastResourceAspectSupport>logTraceInfo(
			() -> "trace test message with Supplier")).isSameAs(aspectSupport);

		verify(mockLogger, times(1)).isTraceEnabled();
		verify(mockLogger, times(1)).trace(eq("trace test message with Supplier"));
	}

	@Test
	public void logWarningWhenWarningsAreDisabled() {

		when(mockLogger.isWarnEnabled()).thenReturn(false);

		assertThat(aspectSupport.<AbstractGemFireAsLastResourceAspectSupport>logWarning("message", "arg"))
			.isSameAs(aspectSupport);

		verify(mockLogger, times(1)).isWarnEnabled();
		verify(mockLogger, never()).warn(anyString());
		verify(mockLogger, never()).warn(anyString(), any(Object.class));
		verify(mockLogger, never()).warn(anyString(), any(Object.class), any(Object.class));
		verify(mockLogger, never()).warn(anyString(), any(Object[].class));
	}

	@Test
	public void logWarningWhenWarningsAreEnabled() {

		when(mockLogger.isWarnEnabled()).thenReturn(true);

		assertThat(aspectSupport.<AbstractGemFireAsLastResourceAspectSupport>logWarning("test %s message", "warning"))
			.isSameAs(aspectSupport);

		verify(mockLogger, times(1)).isWarnEnabled();
		verify(mockLogger, times(1)).warn(eq("test warning message"),
			VariableArgumentMatcher.varArgThat("warning"));
	}

	@Test
	public void logErrorWhenErrorsAreDisabled() {

		when(mockLogger.isErrorEnabled()).thenReturn(false);

		assertThat(aspectSupport.<AbstractGemFireAsLastResourceAspectSupport>logError("message", "arg"))
			.isSameAs(aspectSupport);

		verify(mockLogger, times(1)).isErrorEnabled();
		verify(mockLogger, never()).error(anyString());
		verify(mockLogger, never()).error(anyString(), any(Object.class));
		verify(mockLogger, never()).error(anyString(), any(Object.class), any(Object.class));
		verify(mockLogger, never()).error(anyString(), any(Object[].class));
	}

	@Test
	public void logErrorWhenErrorsAreEnabled() {

		when(mockLogger.isErrorEnabled()).thenReturn(true);

		assertThat(aspectSupport.<AbstractGemFireAsLastResourceAspectSupport>logError("test %s message", "error"))
			.isSameAs(aspectSupport);

		verify(mockLogger, times(1)).isErrorEnabled();
		verify(mockLogger, times(1)).error(eq("test error message"),
			VariableArgumentMatcher.varArgThat("error"));
	}

	@Test
	public void resolveContextReturnsConfiguredContext() throws NamingException {

		Context mockContext = mock(Context.class);

		doReturn(mockContext).when(aspectSupport).getContext();

		assertThat(aspectSupport.resolveContext()).isSameAs(mockContext);

		verify(aspectSupport, never()).newInitialContext(any(Hashtable.class));
		verifyZeroInteractions(mockContext);
	}

	@Test
	@SuppressWarnings("all")
	public void resolveContextReturnsNewInitialContext() throws NamingException {

		Hashtable<String, Object> mockEnvironment = new Hashtable<>();

		InitialContext mockContext = mock(InitialContext.class);

		doReturn(mockContext).when(aspectSupport).newInitialContext(any(Hashtable.class));
		doReturn(mockEnvironment).when(aspectSupport).resolveEnvironment();

		assertThat(aspectSupport.resolveContext()).isEqualTo(mockContext);

		verify(aspectSupport, times(1)).getContext();
		verify(aspectSupport, times(1)).newInitialContext(eq(mockEnvironment));
		verifyZeroInteractions(mockContext);
	}

	@Test
	public void resolveContextHandlesNamingExceptionAndReturnsGemFireJndiContext() throws NamingException {

		Context mockContext = mock(Context.class);

		GemFireCache mockGemFireCache = mock(GemFireCache.class);

		Hashtable<String, Object> mockEnvironment = new Hashtable<>();

		doThrow(new NamingException("TEST")).when(aspectSupport).newInitialContext(any(Hashtable.class));
		doReturn(mockEnvironment).when(aspectSupport).resolveEnvironment();
		doReturn(mockGemFireCache).when(aspectSupport).resolveGemFireCache();
		doReturn(mockContext).when(mockGemFireCache).getJNDIContext();

		assertThat(aspectSupport.resolveContext()).isEqualTo(mockContext);

		verify(aspectSupport, times(1)).getContext();
		verify(aspectSupport, times(1)).resolveEnvironment();
		verify(aspectSupport, times(1)).newInitialContext(eq(mockEnvironment));
		verify(aspectSupport, times(1)).resolveGemFireCache();
		verify(mockGemFireCache, times(1)).getJNDIContext();
	}

	@Test(expected = IllegalStateException.class)
	public void resolveContextHandlesNamingExceptionAndThrowsIllegalStateException() throws NamingException {

		Hashtable<String, Object> testEnvironment = new Hashtable<>();

		testEnvironment.put("key", "test");

		doThrow(new NamingException("TEST")).when(aspectSupport).newInitialContext(any(Hashtable.class));
		doReturn(testEnvironment).when(aspectSupport).resolveEnvironment();
		doReturn(null).when(aspectSupport).resolveGemFireCache();

		try {
			aspectSupport.resolveContext();
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessage(
				"Failed to initialize an %1$s with the provided Environment configuration [%2$s]",
					InitialContext.class.getName(), testEnvironment.toString());

			assertThat(expected).hasCauseInstanceOf(NamingException.class);
			assertThat(expected.getCause()).hasMessage("TEST");
			assertThat(expected.getCause()).hasNoCause();

			throw expected;
		}
		finally {
			verify(aspectSupport, times(1)).getContext();
			verify(aspectSupport, times(1)).resolveEnvironment();
			verify(aspectSupport, times(1)).newInitialContext(eq(testEnvironment));
			verify(aspectSupport, times(1)).resolveGemFireCache();
		}
	}

	@Test
	public void resolveEnvironmentContainsInitialContextFactoryAndProviderUrl() {

		doReturn("org.example.test.naming.AppServerContextFactory").when(aspectSupport)
			.getInitialContextFactory();

		doReturn("jndi:rmi://java/comp:jndi/context").when(aspectSupport).getProviderUrl();

		Hashtable<?, ?> resolvedEnvironment = aspectSupport.resolveEnvironment();

		assertThat(resolvedEnvironment).isNotNull();
		assertThat(resolvedEnvironment).hasSize(2);
		assertThat(resolvedEnvironment.containsKey(Context.INITIAL_CONTEXT_FACTORY)).isTrue();
		assertThat(resolvedEnvironment.get(Context.INITIAL_CONTEXT_FACTORY))
			.isEqualTo("org.example.test.naming.AppServerContextFactory");
		assertThat(resolvedEnvironment.containsKey(Context.PROVIDER_URL)).isTrue();
		assertThat(resolvedEnvironment.get(Context.PROVIDER_URL)).isEqualTo("jndi:rmi://java/comp:jndi/context");
	}

	@Test
	public void resolveGemFireCacheReturnsConfiguredGemFireCache() {

		GemFireCache mockGemFireCache = mock(GemFireCache.class);

		doReturn(mockGemFireCache).when(aspectSupport).getGemFireCache();

		assertThat(aspectSupport.resolveGemFireCache()).isSameAs(mockGemFireCache);

		verify(aspectSupport, times(1)).getGemFireCache();
	}

	@Test
	public void resolveGemFireCacheReturnsNull() {

		assertThat(aspectSupport.resolveGemFireCache()).isNull();

		verify(aspectSupport, times(1)).getGemFireCache();
	}

	@Test
	@SuppressWarnings("all")
	public void resolveGemFireJcaResourceAdapterJndiNameReturnsConfiguredJndiName() {

		doReturn("java/comp:gemfire/jca/resourceAdapter").when(aspectSupport)
			.getGemFireJcaResourceAdapterJndiName();

		assertThat(aspectSupport.resolveGemFireJcaResourceAdapterJndiName())
			.isEqualTo("java/comp:gemfire/jca/resourceAdapter");

		verify(aspectSupport, times(1)).getGemFireJcaResourceAdapterJndiName();
	}

	@Test
	public void resolveGemFireJcaResourceAdapterJndiNameReturnsDefaultJndiName() {
		assertThat(aspectSupport.getGemFireJcaResourceAdapterJndiName()).isNull();
		assertThat(aspectSupport.resolveGemFireJcaResourceAdapterJndiName())
			.isEqualTo(AbstractGemFireAsLastResourceAspectSupport.DEFAULT_GEMFIRE_JCA_RESOURCE_ADAPTER_JNDI_NAME);
	}

	@Test
	public void withThrowOnErrorAndIsThrowOnError() {
		assertThat(aspectSupport.isThrowOnError()).isFalse();
		assertThat(aspectSupport.<AbstractGemFireAsLastResourceAspectSupport>withThrowOnError(true)).isSameAs(aspectSupport);
		assertThat(aspectSupport.isThrowOnError()).isTrue();
		assertThat(aspectSupport.<AbstractGemFireAsLastResourceAspectSupport>withThrowOnError(false)).isSameAs(aspectSupport);
		assertThat(aspectSupport.isThrowOnError()).isFalse();
	}

	// TODO refactor this BS; damn you Mockito for your inability to match Varargs completely/reliably; WTF!
	protected static final class VariableArgumentMatcher<T> implements ArgumentMatcher<T>, VarargMatcher {

		protected static Object[] varArgThat(Object... expectedArguments) {
			return argThat(new VariableArgumentMatcher<>(expectedArguments));
		}

		private Object[] expectedArguments;

		@SuppressWarnings("unchecked")
		protected VariableArgumentMatcher(Object... expectedArguments) {
			this.expectedArguments = Optional.ofNullable(expectedArguments)
				.orElseThrow(() -> newIllegalArgumentException("Expected arguments must not be null"));
		}

		@Override
		@SuppressWarnings("unchecked")
		public boolean matches(T actualArgument) {
			return asList(this.expectedArguments).containsAll(asList(actualArgument));
		}

		private List<Object> asList(Object argument) {
			return Arrays.asList(toArray(argument));
		}

		private Object[] toArray(Object argument) {
			return (argument instanceof Object[] ? (Object[]) argument : new Object[] { argument });
		}
	}
}
