/*
 * Copyright 2010-2019 the original author or authors.
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

import org.springframework.data.gemfire.repository.GemfireRepository;
import org.springframework.data.gemfire.repository.Query;

/**
 * The PlantRepository class is a Repository extension for accessing and storing Plants.
 * Note, this Spring GemFire Repository extension incorrectly maps Plants to the Plants Region on purpose
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.repository.GemfireRepository
 * @see org.springframework.data.gemfire.repository.Query
 * @since 1.4.0
 */
@SuppressWarnings("unused")
public interface PlantRepository extends GemfireRepository<Plant, String> {

	Animal findByName(String name);

	@Query("SELECT * FROM /Placeholder x WHERE x.name = $1")
	Animal findBy(String name);

}
