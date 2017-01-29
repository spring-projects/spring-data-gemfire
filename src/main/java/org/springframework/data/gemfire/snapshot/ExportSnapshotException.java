/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.snapshot;

/**
 * The ExportSnapshotException class is a RuntimeException indicating an error occurred while saving a snapshhot
 * of GemFire's Cache Regions.
 *
 * @author John Blum
 * @see java.lang.RuntimeException
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public class ExportSnapshotException extends RuntimeException {

	public ExportSnapshotException() {
	}

	public ExportSnapshotException(String message) {
		super(message);
	}

	public ExportSnapshotException(Throwable cause) {
		super(cause);
	}

	public ExportSnapshotException(String message, Throwable cause) {
		super(message, cause);
	}

}
