/*
 * Copyright 2016-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation.test.entities;

import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.mapping.annotation.Indexed;
import org.springframework.data.gemfire.mapping.annotation.LuceneIndexed;
import org.springframework.data.gemfire.mapping.annotation.PartitionRegion;

/**
 * {@link PartitionRegionEntity} persistent entity stored in the "Customers"
 * {@link org.apache.geode.cache.DataPolicy#PERSISTENT_PARTITION} {@link org.apache.geode.cache.Region}.
 *
 * @author John Blum
 * @since 1.9.0
 */
@PartitionRegion(name = "Customers", ignoreIfExists = false, persistent = true, redundantCopies = 1,
	fixedPartitions = {
		@PartitionRegion.FixedPartition(name = "one", primary = true, numBuckets = 16),
		@PartitionRegion.FixedPartition(name = "two", numBuckets = 21)
	}
)
@SuppressWarnings("unused")
public class PartitionRegionEntity {

	@Id
	private Long id;

	@Indexed(expression = "first_name", from = "/LoyalCustomers", type = IndexType.FUNCTIONAL)
	private String firstName;

	@Indexed(name = "LastNameIdx", expression = "surname")
	private String lastName;

	@LuceneIndexed("TitleLuceneIdx")
	private String title;

}
