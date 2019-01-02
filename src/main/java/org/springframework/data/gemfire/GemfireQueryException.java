/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire;

import org.apache.geode.cache.query.QueryException;
import org.apache.geode.cache.query.QueryExecutionTimeoutException;
import org.apache.geode.cache.query.QueryInvalidException;
import org.springframework.dao.InvalidDataAccessResourceUsageException;

/**
 * GemFire-specific subclass of {@link InvalidDataAccessResourceUsageException} thrown on invalid
 * OQL query syntax.
 *
 * @author Costin Leau
 */
@SuppressWarnings("serial")
public class GemfireQueryException extends InvalidDataAccessResourceUsageException {

	public GemfireQueryException(String message, QueryException ex) {
		super(message, ex);
	}

	public GemfireQueryException(QueryException ex) {
		super(ex.getMessage(), ex);
	}

	public GemfireQueryException(String message, QueryExecutionTimeoutException ex) {
		super(message, ex);
	}

	public GemfireQueryException(QueryExecutionTimeoutException ex) {
		super(ex.getMessage(), ex);
	}

	public GemfireQueryException(String message, QueryInvalidException ex) {
		super(message, ex);
	}

	public GemfireQueryException(QueryInvalidException ex) {
		super(ex.getMessage(), ex);
	}

	public GemfireQueryException(String message, RuntimeException ex) {
		super(message, ex);
	}

	public GemfireQueryException(RuntimeException ex) {
		super(ex.getMessage(), ex);
	}
}
