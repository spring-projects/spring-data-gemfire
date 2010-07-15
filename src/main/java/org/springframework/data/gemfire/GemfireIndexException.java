/*
 * Copyright 2010 the original author or authors.
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

import org.springframework.dao.DataIntegrityViolationException;

import com.gemstone.gemfire.cache.query.IndexCreationException;
import com.gemstone.gemfire.cache.query.IndexExistsException;
import com.gemstone.gemfire.cache.query.IndexInvalidException;
import com.gemstone.gemfire.cache.query.IndexMaintenanceException;
import com.gemstone.gemfire.cache.query.IndexNameConflictException;

/**
 * Gemfire-specific subclass thrown on index creation.
 *  
 * @author Costin Leau
 */
public class GemfireIndexException extends DataIntegrityViolationException {

	public GemfireIndexException(IndexCreationException ex) {
		super(ex.getMessage(), ex);
	}

	public GemfireIndexException(IndexExistsException ex) {
		super(ex.getMessage(), ex);
	}

	public GemfireIndexException(IndexNameConflictException ex) {
		super(ex.getMessage(), ex);
	}

	public GemfireIndexException(IndexMaintenanceException ex) {
		super(ex.getMessage(), ex);
	}

	public GemfireIndexException(IndexInvalidException ex) {
		super(ex.getMessage(), ex);
	}
}
