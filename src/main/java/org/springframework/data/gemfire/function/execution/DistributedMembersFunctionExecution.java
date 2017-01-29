/*
 * Copyright 2002-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function.execution;

import java.util.Set;

import org.apache.geode.cache.execute.Execution;
import org.apache.geode.cache.execute.FunctionService;
import org.apache.geode.distributed.DistributedMember;

/**
 * @author David Turanski
 *
 */
class DistributedMembersFunctionExecution extends AbstractFunctionExecution {

	private final Set<DistributedMember> distributedMembers;
   /**
    *
    * @param distributedMembers
    */
	public DistributedMembersFunctionExecution(Set<DistributedMember> distributedMembers ) {
		super( );
		this.distributedMembers = distributedMembers;
	}

	@Override
	protected Execution getExecution() {
		return FunctionService.onMembers(this.distributedMembers);
	}

}
