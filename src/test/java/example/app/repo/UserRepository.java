/*
 * Copyright 2020 the original author or authors.
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
package example.app.repo;

import java.util.List;
import java.util.Set;

import org.springframework.data.gemfire.repository.GemfireRepository;

import example.app.model.User;

/**
 * Spring Data [Geode] {@link GemfireRepository} used to perform basic CRUD and simple OQL query data access operations
 * on {@link User} objects to and from an Apache Geode {@link org.apache.geode.cache.GemFireCache}
 * {@link org.apache.geode.cache.Region}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.springframework.data.gemfire.repository.GemfireRepository
 * @see example.app.model.User
 * @since 2.5.0
 */
@SuppressWarnings("unused")
public interface UserRepository extends GemfireRepository<User, Integer> {

	List<User> findByIdInOrderById(Integer... identifiers);

	//@Query("SELECT u FROM /Users u WHERE u.id IN ($1) ORDER BY u.id")
	List<User> findByIdInOrderById(Set<Integer> identifiers);

	List<User> findByNameInOrderByName(String... usersnames);

	List<User> findByNameInOrderByName(Set<String> usernames);

}
