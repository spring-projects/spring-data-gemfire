/*
 * Copyright 2019-2020 the original author or authors.
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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;

import org.springframework.context.Lifecycle;

/**
 * Unit Tests for {@link CompositeLifecycle}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.context.Lifecycle
 * @see org.springframework.data.gemfire.support.CompositeLifecycle
 * @since 2.2.0
 */
public class CompositeLifecycleUnitTests {

	private CompositeLifecycle compositeLifecycle;

	@Before
	public void setup() {
		this.compositeLifecycle = new CompositeLifecycle();
	}

	@Test
	public void addLifecycleObjectToCompositeReturnsTrue() {

		Lifecycle mockLifecycle = mock(Lifecycle.class);

		assertThat(this.compositeLifecycle.add(mockLifecycle)).isTrue();
		assertThat(this.compositeLifecycle).isNotEmpty();
		assertThat(this.compositeLifecycle).hasSize(1);
		assertThat(this.compositeLifecycle).containsExactly(mockLifecycle);
	}

	@Test
	public void addNullReturnsFalse() {

		assertThat(this.compositeLifecycle.add(null)).isFalse();
		assertThat(this.compositeLifecycle).isEmpty();
		assertThat(this.compositeLifecycle).hasSize(0);
	}

	@Test
	public void removeLifecycleObjectReturnsTrue() {

		Lifecycle mockLifecycle = mock(Lifecycle.class);

		assertThat(this.compositeLifecycle.add(mockLifecycle)).isTrue();
		assertThat(this.compositeLifecycle).isNotEmpty();
		assertThat(this.compositeLifecycle).hasSize(1);
		assertThat(this.compositeLifecycle.remove(mockLifecycle)).isTrue();
		assertThat(this.compositeLifecycle).isEmpty();
		assertThat(this.compositeLifecycle).hasSize(0);
	}

	@Test
	public void removeNullReturnsFalse() {

		assertThat(this.compositeLifecycle).isEmpty();
		assertThat(this.compositeLifecycle).hasSize(0);
		assertThat(this.compositeLifecycle.remove(null)).isFalse();
	}

	@Test
	public void isAutoStartupReturnsTrue() {
		assertThat(this.compositeLifecycle.isAutoStartup()).isTrue();
	}

	@Test
	public void isRunningWithAllRunningLifecycleObjectsReturnsTrue() {

		Lifecycle mockLifecycleOne = mock(Lifecycle.class);
		Lifecycle mockLifecycleTwo = mock(Lifecycle.class);

		when(mockLifecycleOne.isRunning()).thenReturn(true);

		assertThat(this.compositeLifecycle.add(mockLifecycleOne)).isTrue();
		assertThat(this.compositeLifecycle.add(mockLifecycleTwo)).isTrue();
		assertThat(this.compositeLifecycle.isRunning()).isTrue();

		verify(mockLifecycleOne, times(1)).isRunning();
		verify(mockLifecycleTwo, never()).isRunning();
	}

	@Test
	public void isRunningWithNoLifecycleObjectReturnsFalse() {
		assertThat(this.compositeLifecycle.isRunning()).isFalse();
	}

	@Test
	public void isRunningWithNoRunningLifecycleObjectsReturnsFalse() {

		Lifecycle mockLifecycleOne = mock(Lifecycle.class);
		Lifecycle mockLifecycleTwo = mock(Lifecycle.class);

		when(mockLifecycleOne.isRunning()).thenReturn(false);
		when(mockLifecycleTwo.isRunning()).thenReturn(false);

		assertThat(this.compositeLifecycle.add(mockLifecycleOne)).isTrue();
		assertThat(this.compositeLifecycle.add(mockLifecycleTwo)).isTrue();
		assertThat(this.compositeLifecycle.isRunning()).isFalse();

		verify(mockLifecycleOne, times(1)).isRunning();
		verify(mockLifecycleTwo, times(1)).isRunning();
	}

	@Test
	public void isRunningWithOneRunningLifecycleObjectsReturnsTrue() {

		Lifecycle mockLifecycleOne = mock(Lifecycle.class);
		Lifecycle mockLifecycleTwo = mock(Lifecycle.class);

		when(mockLifecycleOne.isRunning()).thenReturn(false);
		when(mockLifecycleTwo.isRunning()).thenReturn(true);

		assertThat(this.compositeLifecycle.add(mockLifecycleOne)).isTrue();
		assertThat(this.compositeLifecycle.add(mockLifecycleTwo)).isTrue();
		assertThat(this.compositeLifecycle.isRunning()).isTrue();

		verify(mockLifecycleOne, times(1)).isRunning();
		verify(mockLifecycleTwo, times(1)).isRunning();
	}

	@Test
	public void startStartsAllLifecycleObjects() {

		Lifecycle mockLifecycleOne = mock(Lifecycle.class);
		Lifecycle mockLifecycleTwo = mock(Lifecycle.class);

		assertThat(this.compositeLifecycle.add(mockLifecycleOne)).isTrue();
		assertThat(this.compositeLifecycle.add(mockLifecycleTwo)).isTrue();

		this.compositeLifecycle.start();

		verify(mockLifecycleOne, times(1)).start();
		verify(mockLifecycleTwo, times(1)).start();
	}

	@Test
	public void stopStopsAllLifecycleObjects() {

		Lifecycle mockLifecycleOne = mock(Lifecycle.class);
		Lifecycle mockLifecycleTwo = mock(Lifecycle.class);

		assertThat(this.compositeLifecycle.add(mockLifecycleOne)).isTrue();
		assertThat(this.compositeLifecycle.add(mockLifecycleTwo)).isTrue();

		this.compositeLifecycle.stop();

		verify(mockLifecycleOne, times(1)).stop();
		verify(mockLifecycleTwo, times(1)).stop();
	}
}
