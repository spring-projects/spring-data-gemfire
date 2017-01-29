/*
 * Copyright 2017-2018 the original author or authors.
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
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import javax.naming.Context;
import javax.naming.NamingException;
import javax.resource.ResourceException;

import org.apache.geode.ra.GFConnection;
import org.apache.geode.ra.GFConnectionFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.slf4j.Logger;

/**
 * Unit tests for {@link GemFireAsLastResourceConnectionAcquiringAspect}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.config.annotation.support.GemFireAsLastResourceConnectionAcquiringAspect
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class GemFireAsLastResourceConnectionAcquiringAspectUnitTests {

	@Mock
	private Context mockContext;

	@Mock
	private GFConnection mockGemFireConnection;

	@Mock
	private GFConnectionFactory mockGemFireConnectionFactory;

	@Mock
	private Logger mockLogger;

	private GemFireAsLastResourceConnectionAcquiringAspect aspect;

	@Before
	public void setup() {
		aspect = spy(new GemFireAsLastResourceConnectionAcquiringAspect());
		when(aspect.getLogger()).thenReturn(mockLogger);
	}

	@Test
	public void connectionAcquiringAspectHasLowerPriorityThanConnectionClosingAspect() {
		assertThat(aspect.getOrder()).isGreaterThan(new GemFireAsLastResourceConnectionClosingAspect().getOrder());
	}

	@Test
	public void doConnectionFactoryGetConnectionReturnsConnection() throws ResourceException {

		when(mockLogger.isTraceEnabled()).thenReturn(true);
		doReturn(mockGemFireConnectionFactory).when(aspect).resolveGemFireConnectionFactory();
		when(mockGemFireConnectionFactory.getConnection()).thenReturn(mockGemFireConnection);

		aspect.doGemFireConnectionFactoryGetConnection();

		assertThat(AbstractGemFireAsLastResourceAspectSupport.GemFireConnectionHolder.get().orElse(null))
			.isEqualTo(mockGemFireConnection);

		verify(aspect, times(1)).resolveGemFireConnectionFactory();
		verify(aspect, times(1)).resolveGemFireJcaResourceAdapterJndiName();
		verify(mockGemFireConnectionFactory, times(1)).getConnection();
		verify(mockLogger, times(1))
			.trace(eq(String.format("Acquiring GemFire Connection from GemFire JCA ResourceAdapter registered at [%s]...",
				GemFireAsLastResourceConnectionAcquiringAspect.DEFAULT_GEMFIRE_JCA_RESOURCE_ADAPTER_JNDI_NAME)));
	}

	@Test
	public void resolveGemFireConnectionFactoryFromAutowiring() {

		when(aspect.getGemFireConnectionFactory()).thenReturn(mockGemFireConnectionFactory);

		assertThat(aspect.resolveGemFireConnectionFactory()).isSameAs(mockGemFireConnectionFactory);

		verify(aspect, times(1)).getGemFireConnectionFactory();
		verify(aspect, never()).resolveGemFireJcaResourceAdapterJndiName();
		verify(aspect, never()).resolveContext();
	}

	@Test
	public void resolveGemFireConnectionFactoryFromJndiContext() throws NamingException {

		when(aspect.getGemFireConnectionFactory()).thenReturn(null);
		doReturn(mockContext).when(aspect).resolveContext();
		when(mockContext.lookup(anyString())).thenReturn(mockGemFireConnectionFactory);

		assertThat(aspect.resolveGemFireConnectionFactory()).isEqualTo(mockGemFireConnectionFactory);

		verify(aspect, times(1)).getGemFireConnectionFactory();
		verify(aspect, times(1)).resolveGemFireJcaResourceAdapterJndiName();
		verify(aspect, times(1)).resolveContext();
		verify(mockContext, times(1))
			.lookup(eq(GemFireAsLastResourceConnectionAcquiringAspect.DEFAULT_GEMFIRE_JCA_RESOURCE_ADAPTER_JNDI_NAME));
	}

	@Test(expected = RuntimeException.class)
	public void resolveGemFireConnectionFactoryFromJndiContextThrowsNamingException() throws NamingException {

		when(aspect.getGemFireConnectionFactory()).thenReturn(null);
		when(aspect.getGemFireJcaResourceAdapterJndiName()).thenReturn("java:comp/gemfire/jca");
		doReturn(mockContext).when(aspect).resolveContext();
		when(mockContext.lookup(anyString())).thenThrow(new NamingException("TEST"));

		try {
			aspect.resolveGemFireConnectionFactory();
		}
		catch (RuntimeException expected) {

			assertThat(expected).hasMessage(
				"Failed to resolve a GFConnectionFactory from the configured JNDI context name [java:comp/gemfire/jca]");

			assertThat(expected).hasCauseInstanceOf(NamingException.class);
			assertThat(expected.getCause()).hasMessage("TEST");
			assertThat(expected.getCause()).hasNoCause();

			throw expected;
		}
		finally {
			verify(aspect, times(1)).getGemFireConnectionFactory();
			verify(aspect, times(1)).resolveGemFireJcaResourceAdapterJndiName();
			verify(aspect, times(1)).resolveContext();
			verify(mockContext, times(1)).lookup(eq("java:comp/gemfire/jca"));
		}
	}
}
