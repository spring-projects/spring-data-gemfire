/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.repository;

import org.springframework.data.domain.Sort;
import org.springframework.data.repository.CrudRepository;

/**
 * Apache Geode extension of the Spring Data {@link CrudRepository} interface.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.springframework.data.repository.CrudRepository
 */
public interface GemfireRepository<T, ID> extends CrudRepository<T, ID> {

	/**
	 * Returns all entities ordered by the given {@link Sort}.
	 *
	 * @param sort {@link Sort} defining the ordering criteria.
	 * @return all entities ordered by the given {@link Sort}.
	 * @see org.springframework.data.repository.PagingAndSortingRepository#findAll(org.springframework.data.domain.Sort)
	 * @see org.springframework.data.domain.Sort
	 * @see java.lang.Iterable
	 */
	Iterable<T> findAll(Sort sort);

	T save(Wrapper<T, ID> wrapper);

}
