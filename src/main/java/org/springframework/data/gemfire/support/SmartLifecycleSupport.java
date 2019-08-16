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

import org.springframework.context.SmartLifecycle;

/**
 * The {@link SmartLifecycleSupport} interface is an extension of Spring's {@link SmartLifecycle} interface
 * providing default, convenient behavior for many of the lifecycle methods as well as a serving
 * as a {@link FunctionalInterface}.
 *
 * @author John Blum
 * @see java.lang.FunctionalInterface
 * @see org.springframework.context.SmartLifecycle
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface SmartLifecycleSupport extends SmartLifecycle {

	boolean DEFAULT_AUTO_STARTUP = true;
	boolean DEFAULT_IS_RUNNING = false;

	int DEFAULT_PHASE = 0;

	@Override
	default boolean isAutoStartup() {
		return DEFAULT_AUTO_STARTUP;
	}

	@Override
	default void stop(Runnable runnable) {
		stop();
		runnable.run();
	}

	@Override
	default void stop() { }

	@Override
	default boolean isRunning() {
		return DEFAULT_IS_RUNNING;
	}

	@Override
	default int getPhase() {
		return DEFAULT_PHASE;
	}
}
