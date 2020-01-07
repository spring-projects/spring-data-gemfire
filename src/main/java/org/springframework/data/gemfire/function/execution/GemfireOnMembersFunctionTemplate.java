/*
 * Copyright 2002-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function.execution;

import java.util.Set;

import org.apache.geode.distributed.DistributedMember;

/**
 * @author David Turanski
 * @see org.springframework.data.gemfire.function.execution.AbstractFunctionTemplate
 * @see org.apache.geode.distributed.DistributedMember
 */
public class GemfireOnMembersFunctionTemplate  extends AbstractFunctionTemplate {

	private final Set<DistributedMember> distributedMembers;
    private final String[] groups;

	public GemfireOnMembersFunctionTemplate (Set<DistributedMember> distributedMembers) {
		this.distributedMembers = distributedMembers;
		this.groups = null;
	}

	public GemfireOnMembersFunctionTemplate (String[] groups) {
		this.distributedMembers = null;
		this.groups = groups;
	}

	public GemfireOnMembersFunctionTemplate () {
		this.distributedMembers = null;
		this.groups = null;
	}

	protected AbstractFunctionExecution getFunctionExecution() {
		if (distributedMembers == null && groups == null) {
			return new AllMembersFunctionExecution();
		} else if (distributedMembers == null) {
			return new GroupMembersFunctionExecution(this.groups);
		}

		return new DistributedMembersFunctionExecution(this.distributedMembers);
	}

}
