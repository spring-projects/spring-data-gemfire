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

package org.springframework.data.gemfire.test.logging.slf4j.logback;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.util.Optional;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicReference;

import org.springframework.util.StringUtils;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.Appender;
import ch.qos.logback.core.AppenderBase;

/**
 * The {@link TestAppender} class is a SLF4J, Logback {@link Appender} implementation used for testing purposes.
 *
 * @author John Blum
 * @see ch.qos.logback.core.Appender
 * @see ch.qos.logback.core.AppenderBase
 * @since 2.0.2
 */
@SuppressWarnings("unused")
public class TestAppender extends AppenderBase<ILoggingEvent> implements Appender<ILoggingEvent> {

	private static final AtomicReference<TestAppender> INSTANCE = new AtomicReference<>(null);

	private static final Stack<String> logMessages = new Stack<>();

	public static TestAppender getInstance() {

		return Optional.ofNullable(INSTANCE.get())
			.orElseThrow(() -> newIllegalStateException("[%s] was not properly configured",
				TestAppender.class.getName()));
	}

	public TestAppender() {
		INSTANCE.compareAndSet(null, this);
	}

	@Override
	protected void append(ILoggingEvent event) {

		Optional.ofNullable(event)
			.map(ILoggingEvent::getFormattedMessage)
			.filter(StringUtils::hasText)
			.ifPresent(logMessages::push);
	}

	public String lastLogMessage() {
		synchronized (logMessages) {
			return (logMessages.empty() ? null : logMessages.pop());
		}
	}

	public void clear() {
		logMessages.clear();
	}
}
