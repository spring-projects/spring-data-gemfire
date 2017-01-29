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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import javax.resource.ResourceException;

import org.apache.geode.ra.GFConnection;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.slf4j.Logger;

/**
 * Unit tests for {@link GemFireAsLastResourceConnectionClosingAspect}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.config.annotation.support.GemFireAsLastResourceConnectionClosingAspect
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class GemFireAsLastResourceConnectionClosingAspectUnitTests {

	private GemFireAsLastResourceConnectionClosingAspect aspect;

	@Mock
	private GFConnection mockGemFireConnection;

	@Mock
	private Logger mockLogger;

	@Before
	public void setup() {
		aspect = spy(new GemFireAsLastResourceConnectionClosingAspect());
		when(aspect.getLogger()).thenReturn(mockLogger);
	}

	@Test
	public void connectionClosingAspectHasHigherPriorityThanConnectionAcquiringAspect() {
		assertThat(aspect.getOrder()).isLessThan(new GemFireAsLastResourceConnectionAcquiringAspect().getOrder());
	}

	@Test
	public void doGemFireConnectionCloseIsSuccessful() throws ResourceException {

		AbstractGemFireAsLastResourceAspectSupport.GemFireConnectionHolder.of(mockGemFireConnection);

		when(mockLogger.isTraceEnabled()).thenReturn(true);

		aspect.doGemFireConnectionClose();;

		verify(mockGemFireConnection, times(1)).close();
		verify(mockLogger, times(1)).trace(eq("Closing GemFire Connection..."));
	}
}
