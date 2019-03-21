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

import java.util.List;

import org.springframework.data.gemfire.repository.GemfireRepository;

/**
 * The ProgrammerRepository class is a Data Access Object (DAO) for Programmer domain objects supporting basic CRUD
 * and Query operations.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.repository.GemfireRepository
 * @see org.springframework.data.gemfire.repository.sample.Programmer
 * @since 1.4.0
 */
@SuppressWarnings("unused")
public interface ProgrammerRepository extends GemfireRepository<Programmer, String> {

	public List<Programmer> findDistinctByProgrammingLanguageOrderByUsernameAsc(String programmingLanguage);

	public List<Programmer> findDistinctByProgrammingLanguageLikeOrderByUsernameAsc(String programmingLanguage);

}
