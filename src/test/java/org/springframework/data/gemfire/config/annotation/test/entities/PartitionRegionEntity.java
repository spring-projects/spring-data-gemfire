/*
 * Copyright 2016 the original author or authors.
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
import org.springframework.data.gemfire.mapping.Indexed;
import org.springframework.data.gemfire.mapping.PartitionRegion;

/**
 * {@link PartitionRegionEntity} persistent entity stored in the "Customers"
 * {@link com.gemstone.gemfire.cache.DataPolicy#PERSISTENT_PARTITION} {@link com.gemstone.gemfire.cache.Region}.
 *
 * @author John Blum
 * @since 1.9.0
 */
@PartitionRegion(name = "Customers", persistent = true, redundantCopies = 1,
	fixedPartitions = {
		@PartitionRegion.FixedPartition(name = "one", primary = true, numBuckets = 16),
		@PartitionRegion.FixedPartition(name = "two", numBuckets = 21)
	}
)
public class PartitionRegionEntity {

	@Id
	private Long id;

	@Indexed(expression = "first_name", from = "/LoyalCustomers", type = IndexType.FUNCTIONAL)
	private String firstName;

	@Indexed(name = "LastNameIdx")
	private String lastName;

}
