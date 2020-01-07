/*
 * Copyright 2017-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.commons.logging.Log;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.beans.factory.BeanFactory;

/**
 * Unit tests for {@link AbstractFactoryBeanSupport}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.springframework.data.gemfire.support.AbstractFactoryBeanSupport
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class AbstractFactoryBeanSupportUnitTests {

	@Mock
	private Log mockLog;

	@Spy
	private TestFactoryBeanSupport<?> factoryBeanSupport;

	@Before
	public void setup() {
		when(factoryBeanSupport.getLog()).thenReturn(mockLog);
	}

	@Test
	public void setAndGetBeanClassLoader() {
		assertThat(factoryBeanSupport.getBeanClassLoader()).isNull();

		ClassLoader mockClassLoader = mock(ClassLoader.class);

		factoryBeanSupport.setBeanClassLoader(mockClassLoader);

		assertThat(factoryBeanSupport.getBeanClassLoader()).isSameAs(mockClassLoader);

		ClassLoader systemClassLoader = ClassLoader.getSystemClassLoader();

		factoryBeanSupport.setBeanClassLoader(systemClassLoader);

		assertThat(factoryBeanSupport.getBeanClassLoader()).isSameAs(systemClassLoader);

		factoryBeanSupport.setBeanClassLoader(null);

		assertThat(factoryBeanSupport.getBeanClassLoader()).isNull();
	}

	@Test
	public void setAndGetBeanFactory() {
		assertThat(factoryBeanSupport.getBeanFactory()).isNull();

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		factoryBeanSupport.setBeanFactory(mockBeanFactory);

		assertThat(factoryBeanSupport.getBeanFactory()).isSameAs(mockBeanFactory);

		factoryBeanSupport.setBeanFactory(null);

		assertThat(factoryBeanSupport.getBeanFactory()).isNull();
	}

	@Test
	public void setAndGetBeanName() {
		assertThat(factoryBeanSupport.getBeanName()).isNullOrEmpty();

		factoryBeanSupport.setBeanName("test");

		assertThat(factoryBeanSupport.getBeanName()).isEqualTo("test");

		factoryBeanSupport.setBeanName(null);

		assertThat(factoryBeanSupport.getBeanName()).isNullOrEmpty();
	}

	@Test
	public void isSingletonDefaultsToTrue() {
		assertThat(factoryBeanSupport.isSingleton()).isTrue();
	}

	@Test
	public void logsDebugWhenDebugIsEnabled() {
		when(mockLog.isDebugEnabled()).thenReturn(true);

		factoryBeanSupport.logDebug("%s log test", "debug");

		verify(mockLog, times(1)).isDebugEnabled();
		verify(mockLog, times(1)).debug(eq("debug log test"));
	}

	@Test
	public void logsInfoWhenInfoIsEnabled() {
		when(mockLog.isInfoEnabled()).thenReturn(true);

		factoryBeanSupport.logInfo("%s log test", "info");

		verify(mockLog, times(1)).isInfoEnabled();
		verify(mockLog, times(1)).info(eq("info log test"));
	}

	@Test
	public void logsWarningWhenWarnIsEnabled() {
		when(mockLog.isWarnEnabled()).thenReturn(true);

		factoryBeanSupport.logWarning("%s log test", "warn");

		verify(mockLog, times(1)).isWarnEnabled();
		verify(mockLog, times(1)).warn(eq("warn log test"));
	}

	@Test
	public void logsWarningWhenErrorIsEnabled() {
		when(mockLog.isErrorEnabled()).thenReturn(true);

		factoryBeanSupport.logError("%s log test", "error");

		verify(mockLog, times(1)).isErrorEnabled();
		verify(mockLog, times(1)).error(eq("error log test"));
	}

	@Test
	public void suppressesDebugLoggingWhenDebugIsDisabled() {
		when(mockLog.isDebugEnabled()).thenReturn(false);

		factoryBeanSupport.logDebug(() -> "test");

		verify(mockLog, times(1)).isDebugEnabled();
		verify(mockLog, never()).debug(any());
	}

	@Test
	public void suppressesInfoLoggingWhenInfoIsDisabled() {
		when(mockLog.isInfoEnabled()).thenReturn(false);

		factoryBeanSupport.logInfo(() -> "test");

		verify(mockLog, times(1)).isInfoEnabled();
		verify(mockLog, never()).info(any());
	}

	@Test
	public void suppressesWarnLoggingWhenWarnIsDisabled() {
		when(mockLog.isWarnEnabled()).thenReturn(false);

		factoryBeanSupport.logWarning(() -> "test");

		verify(mockLog, times(1)).isWarnEnabled();
		verify(mockLog, never()).warn(any());
	}

	@Test
	public void suppressesErrorLoggingWhenInfoIsDisabled() {
		when(mockLog.isErrorEnabled()).thenReturn(false);

		factoryBeanSupport.logError(() -> "test");

		verify(mockLog, times(1)).isErrorEnabled();
		verify(mockLog, never()).error(any());
	}

	private static class TestFactoryBeanSupport<T> extends AbstractFactoryBeanSupport<T> {

		@Override
		public T getObject() throws Exception {
			return null;
		}

		@Override
		public Class<?> getObjectType() {
			return Object.class;
		}
	}
}
