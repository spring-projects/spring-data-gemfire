/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire.repository.sample;

import org.springframework.data.gemfire.mapping.annotation.Region;
import org.springframework.util.StringUtils;

/**
 * The Programmer class is a User representing/modeling a software engineer/developer.
 *
 * @author John J. Blum
 * @see Region
 * @see org.springframework.data.gemfire.repository.sample.User
 * @since 1.4.0
 */
@Region("Programmers")
@SuppressWarnings("unused")
public class Programmer extends User {

	protected static final String DEFAULT_PROGRAMMING_LANGUAGE = "?";

	private String programmingLanguage;

	public Programmer(final String username) {
		super(username);
	}

	public String getProgrammingLanguage() {
		return (StringUtils.hasText(programmingLanguage) ? programmingLanguage : DEFAULT_PROGRAMMING_LANGUAGE);
	}

	public void setProgrammingLanguage(final String programmingLanguage) {
		this.programmingLanguage = programmingLanguage;
	}

	@Override
	public String toString() {
		return String.format("%1$s programs in '%2$s.", getUsername(), getProgrammingLanguage());
	}

}
