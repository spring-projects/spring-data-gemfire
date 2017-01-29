/*
 * Copyright 2010-2018 the original author or authors.
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

import org.apache.geode.GemFireCheckedException;
import org.apache.geode.GemFireException;
import org.springframework.dao.UncategorizedDataAccessException;

/**
 * GemFire-specific subclass of UncategorizedDataAccessException, for GemFire system errors that do not match any concrete <code>org.springframework.dao</code> exceptions.
 *
 * @author Costin Leau
 */
@SuppressWarnings("serial")
public class GemfireSystemException extends UncategorizedDataAccessException {

	public GemfireSystemException(GemFireCheckedException ex) {
		super(ex.getMessage(), ex);
	}

	public GemfireSystemException(GemFireException ex) {
		super(ex.getMessage(), ex);
	}

	public GemfireSystemException(RuntimeException ex) {
		super(ex.getMessage(), ex);
	}
}
