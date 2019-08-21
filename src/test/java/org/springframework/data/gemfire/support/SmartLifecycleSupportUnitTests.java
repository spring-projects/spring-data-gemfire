/*
 * Copyright 2018-2019 the original author or authors.
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
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;

import org.springframework.context.SmartLifecycle;

/**
 * Unit tests for {@link SmartLifecycleSupport}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.support.SmartLifecycleSupport
 * @since 1.0.0
 */
public class SmartLifecycleSupportUnitTests {

	@Test
	public void isAutoStartupReturnsTrueByDefault() {

		SmartLifecycle smartLifecycle = mock(SmartLifecycleSupport.class);

		when(smartLifecycle.isAutoStartup()).thenCallRealMethod();

		assertThat(smartLifecycle.isAutoStartup()).isTrue();

		verify(smartLifecycle, times(1)).isAutoStartup();
	}

	@Test
	public void stopWithRunnableCallsStopCallsRunnableRun() {

		Runnable mockRunnable = mock(Runnable.class);

		SmartLifecycle smartLifecycle = mock(SmartLifecycleSupport.class);

		doCallRealMethod().when(smartLifecycle).stop(any(Runnable.class));

		smartLifecycle.stop(mockRunnable);

		verify(smartLifecycle, times(1)).stop();
		verify(mockRunnable, times(1)).run();
	}

	@Test
	public void isRunningReturnsFalse() {

		SmartLifecycle smartLifecycle = mock(SmartLifecycleSupport.class);

		when(smartLifecycle.isRunning()).thenCallRealMethod();

		assertThat(smartLifecycle.isRunning()).isFalse();

		verify(smartLifecycle, times(1)).isRunning();
	}

	@Test
	public void getPhaseReturnsDefaultPhase() {

		SmartLifecycle smartLifecycle = mock(SmartLifecycleSupport.class);

		when(smartLifecycle.getPhase()).thenCallRealMethod();

		assertThat(smartLifecycle.getPhase()).isEqualTo(SmartLifecycleSupport.DEFAULT_PHASE);

		verify(smartLifecycle, times(1)).getPhase();
	}
}
