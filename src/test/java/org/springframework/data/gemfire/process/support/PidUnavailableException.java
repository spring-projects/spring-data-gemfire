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

package org.springframework.data.gemfire.process.support;

/**
 * The PidUnavailableException class is a RuntimeException indicating that the process ID (PID) is unobtainable for
 * the current process.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 1.5.0
 */
@SuppressWarnings("unused")
public class PidUnavailableException extends RuntimeException {

	public PidUnavailableException() {
	}

	public PidUnavailableException(final String message) {
		super(message);
	}

	public PidUnavailableException(final Throwable cause) {
		super(cause);
	}

	public PidUnavailableException(final String message, final Throwable cause) {
		super(message, cause);
	}

}
