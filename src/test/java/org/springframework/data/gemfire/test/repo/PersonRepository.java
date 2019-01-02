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

package org.springframework.data.gemfire.test.repo;

import org.springframework.data.gemfire.test.model.Person;
import org.springframework.data.repository.CrudRepository;

/**
 * The {@link PersonRepository} interface defines a Spring Data {@link CrudRepository} used by applications
 * to perform basic CRUD and querying data access operations on {@link Person people}.
 *
 * @author John Blum
 * @see java.lang.Long
 * @see org.springframework.data.gemfire.test.model.Person
 * @see org.springframework.data.repository.CrudRepository
 * @since 2.0.0
 */
public interface PersonRepository extends CrudRepository<Person, Long> {

}
