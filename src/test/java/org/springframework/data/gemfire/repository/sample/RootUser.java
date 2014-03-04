/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.repository.sample;

import org.springframework.data.gemfire.mapping.Region;

/**
 * The RootUser class represents an authorized administrative user of a service or computer system, etc.
 * <p/>
 * @author John Blum
 * @see org.springframework.data.gemfire.mapping.Region
 * @see org.springframework.data.gemfire.repository.sample.User
 * @since 1.3.4
 */
@Region("/Local/Admin/Users")
public class RootUser extends User {

	public RootUser(final String username) {
		super(username);
	}

	@Override
	public String toString() {
		return String.format("Root User '%1$s'", getUsername());
	}

}
