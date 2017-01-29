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


import org.apache.geode.distributed.DistributedMember;

/**
 * @author David Turanski
 * @see org.springframework.data.gemfire.function.execution.AbstractFunctionTemplate
 * @see org.apache.geode.distributed.DistributedMember
 */
public class GemfireOnMemberFunctionTemplate  extends AbstractFunctionTemplate {

	private final DistributedMember distributedMember;
    private final String[] groups;

	public GemfireOnMemberFunctionTemplate (DistributedMember distributedMember) {
		this.distributedMember = distributedMember;
		this.groups = null;
	}

	public GemfireOnMemberFunctionTemplate (String[] groups) {
		this.distributedMember = null;
		this.groups = groups;
	}

	public GemfireOnMemberFunctionTemplate () {
		this.distributedMember = null;
		this.groups = null;
	}


	protected AbstractFunctionExecution getFunctionExecution() {
		if (distributedMember == null && groups == null) {
			return new DefaultMemberFunctionExecution();
		} else if (distributedMember == null) {
			return new GroupMemberFunctionExecution(this.groups);
		}

		return new DistributedMemberFunctionExecution(this.distributedMember);
	}

}
